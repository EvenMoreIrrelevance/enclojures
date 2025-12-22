(ns emi.enclojures.xfs
  (:require [emi.enclojures.conditions :as conditions]
            [emi.enclojures.destructure :refer [defnc]]))

(defn -fixpoint
  [eq f val]
  (let [val' (f val)]
    (if (eq val' val)
      val'
      (recur eq f val'))))

(defn -preserve-reduced
  [x]
  (if (reduced? x)
    (reduced x)
    x))

(defn -sresolve
  [in]
  (cond
    (or (class? in) (var? in)) in
    (symbol? in) (resolve in)
    :else nil))

(defprotocol _XfInliners
  (-state [_ xf args])
  (-init [_ state rf initform])
  (-step [_ state rf acc in stepform])
  (-finalize [_ state rf out finalform])
  (-uses-rf? [_]))

(defn -form-var [form]
  (-sresolve
    (if (sequential? form)
      (first form)
      form)))

(defmulti -xf-inliners -form-var)
(defmulti -xf-expand -form-var)

(defmethod -xf-inliners :default [_] nil)
(defmethod -xf-expand :default [form] form)

(defn -default-state
  [xf args]
  (mapv vector
    (map #(str (name xf) "-arg-" % "-") (range))
    args))

(defn -default-init
  ([_state initform] initform)
  ([_state _rf initform] initform))
(defn -default-finalize
  ([_state _rf _out finalform] finalform)
  ([_state _out finalform] finalform))

(conditions/defcondition ::xf-step-not-registered ::conditions/input-error)

(defnc register-xf!*
  [thevar {:keys [(or state -default-state)
                  (or init -default-init)
                  (or finalize -default-finalize)
                  (or step (conditions/raise ::xf-step-not-registered))
                  (boolean uses-rf?)]}]
  (let [impl
        (reify
          _XfInliners
          (-uses-rf? [_] uses-rf?)

          (-state [_ xf args]
            (state xf args))
          (-init [_ state rf initform]
            (if uses-rf?
              (init state rf initform)
              (init state initform)))
          (-step [_ state rf acc in stepform]
            (if uses-rf?
              (step state rf acc in stepform)
              (step state acc in stepform)))
          (-finalize [_ state rf out finalform]
            (if uses-rf?
              (finalize state rf out finalform)
              (finalize state out finalform))))]
    (defmethod -xf-inliners thevar [_] impl)))

(defn register-xf-expander!*
  [thevar fun]
  (defmethod -xf-expand thevar [form]
    (apply fun (when (sequential? form) (next form)))))

(defn partition-when
  [partition?]
  (fn partition-when-transducer [rf]
    (let [!chunk (java.util.ArrayList.)
          chunk-step (fn chunk-step [acc]
                       (rf acc
                         (let [v (vec (.toArray !chunk))]
                           (.clear !chunk)
                           v)))]
      (fn partition-when-rf
        ([] (rf))
        ([out]
         (rf
           (if (zero? (.size !chunk))
             out
             (unreduced (chunk-step out)))))
        ([acc in]
         (let [cs (.size !chunk)
               acc'
               (if (or
                     (zero? cs)
                     (not (partition? (.get !chunk (dec cs)) in)))
                 acc
                 (chunk-step acc))]
           (when-not (reduced? acc') (.add !chunk in))
           acc'))))))

(comment
  (into [] (partition-when >) [1 2 3 1 2 3 1])

  *e)

(defn -tree-rf-step
  [rf node? children]
  (fn tree-rf-step [acc in]
    (-preserve-reduced
      (if (node? in)
        (reduce tree-rf-step acc (children in))
        (rf acc in)))))

(defn tree
  ([node?]
   (tree node? identity))
  ([node? children]
   (fn [rf]
     (let [step (-tree-rf-step rf node? children)]
       (fn tree-rf
         ([] (rf))
         ([o] (rf (unreduced o)))
         ([acc in]
          (step acc in)))))))

(declare xf)

(defn -xf-chain
  [args]
  (let [see-through? #{(var comp) (var xf)}]
    (-fixpoint
      (fn -eq-by-count? [a b] (= (count a) (count b)))
      (fn expand [args]
        (into [] (comp
                   (map #(-fixpoint identical? -xf-expand %))
                   (tree
                     #(and (seq? %) (see-through? (-sresolve (first %))))
                     next))
          args))
      args)))

(defn -ensure-sequential
  [x]
  (if-not (sequential? x)
    (list x)
    x))

;;;; The idea here is that sth like `cat` can sensibly expand if it can work with its own `rf`,
;;;; allowing to inline, say, [(keep <...>) cat].
;;;; However, uninlinable xforms should stay isolated.
(defn -xf-chunks
  []
  (partition-when
    (fn xf-chunks-partitioner [old curr]
      (or
        (not (-xf-inliners curr))
        (let [i (-xf-inliners old)]
          (or
            (not i)
            (-uses-rf? i)))))))

(defn -pull-args
  [[xf & args]]
  (let [args_ (repeatedly (count args) gensym)]
    {:bindings (interleave args_ args)
     :form `(~xf ~@args_)}))

(defn -xf-chunk->type-and-fn
  [xfs]
  (if (= 1 (count (-ensure-sequential xfs)))
    {:type nil
     :fn (first xfs)}
    (let [bindings-and-xfs (mapv #(-pull-args (-ensure-sequential %)) xfs)
          bindings (mapcat :bindings bindings-and-xfs)
          xfs (map :form bindings-and-xfs)
          inliners (map -xf-inliners xfs)
          states (mapv (fn [i [h & args]] (-state i h args)) inliners xfs)
          init-lists (mapv (fn [specs]
                             (mapv (fn [[h _v m]]
                                     (with-meta (gensym (symbol h)) m))
                               specs))
                       states)
          steps (rseq (mapv vector inliners init-lists))
          acc_ (gensym "acc")
          in_ (gensym "in")
          out_ (gensym "out")
          rf_ (gensym "rf")
          self_ `_self#
          typname (with-meta (symbol (str "_Xf" (random-uuid))) {:private true})]
      {:type
       `(deftype ~typname [~rf_ ~@(apply concat init-lists)]
          clojure.lang.IFn
          (~'invoke
           [~self_]
           ~(reduce (fn [initform [inliner state]] (-init inliner state rf_ initform))
              `(~rf_) steps))
          (~'invoke
           [~self_ ~out_]
           ~(reduce (fn [finalform [inliner state]] (-finalize inliner state rf_ out_ finalform))
              `(~rf_ ~out_) steps))
          (~'invoke
           [~self_ ~acc_ ~in_]
           ~(reduce (fn [stepform [inliner state]] (-step inliner state rf_ acc_ in_ stepform))
              `(~rf_ ~acc_ ~in_) steps))
          (~'applyTo
           [~self_ seq#]
           (clojure.lang.AFn/applyToHelper ~self_ seq#)))
       :fn
       `(let [~@bindings]
          (fn [rf#] (new ~typname rf# ~@(map (fn [[_h v _m]] v) (apply concat states)))))})))

(defn -xf*
  [& args]
  (let [types-and-fns
        (into [] (comp
                   (-xf-chunks)
                   (map -xf-chunk->type-and-fn))
          (-xf-chain args))]
    (run! eval (keep :type types-and-fns))
    `(comp ~@(map :fn types-and-fns))))

(comment
  (-xf-expand `(remove odd?))

  (-xf-chain
    '((tree #(not (number? %)))
      (lag max)
      (map inc)
      (take 3)
      (lag +)
      (cartesian [1 2 3] [4 5])
      cat))

  (into [] (comp
             (-xf-chunks)
             (map -xf-chunk->type-and-fn))
    (-xf-chain
      '[(mapcat dec)
        (remove odd?)
        (map inc)
        (partition-all 3)
        (drop 9)
        (dedupe)
        (comp
          cat
          cat
          (keep 5))]))

  (apply -xf*
    '[(mapcat dec)
      (remove odd?)
      (map inc)
      (partition-all 3)
      (drop 9)
      (dedupe)
      (comp
        cat
        cat
        (keep odd?))])
  *e)

(defmacro xf
  [& args]
  (apply -xf* args))

(register-xf!* (var distinct)
  {:state (fn [_ []]
            [["distinct-found" `(transient #{}) {:volatile-mutable true}]])
   :step (fn [[found] acc in stepform]
           `(if (contains? ~found ~in)
              ~acc
              (do
                (set! ~found (assoc! ~found ~in))
                ~stepform)))
   :finalize (fn [[found] _out outform]
               `(do
                  (set! ~found nil)
                  ~outform))})

(register-xf!* (var map)
  {:step (fn [[f_] _acc in stepform]
           `(let [~in (~f_ ~in)]
              ~stepform))})

(register-xf!* (var keep)
  {:step (fn [[f_] acc in stepform]
           `(if-some [~in (~f_ ~in)]
              ~stepform
              ~acc))})

(register-xf!* (var filter)
  {:step (fn [[pred_] acc in stepform]
           `(if (~pred_ ~in)
              ~stepform
              ~acc))})

(register-xf-expander!* (var remove)
  (fn remove-expander [f]
    `(filter (complement ~f))))

(register-xf!* (var take-while)
  {:step (fn [[continue?_] acc in stepform]
           `(if (~continue?_ ~in)
              ~stepform
              (reduced ~acc)))})

(register-xf!* (var drop-while)
  {:state (fn [_ [drop?]]
            [["dw-drop?" drop?]
             ["dw-done?" false {:volatile-mutable true}]])
   :step (fn [[drop?_ done-dropping?_] acc in stepform]
           `(if (or
                  ~done-dropping?_
                  (when-not (~drop?_ ~in) (set! ~done-dropping?_ true)))
              ~stepform
              ~acc))})

(register-xf!* (var dedupe)
  {:state (fn [_ []]
            [["dedupe-latest" :clojure.core/none {:volatile-mutable true} #_"mirror current behavior of dedupe."]])
   :step (fn [[latest_] acc in stepform]
           `(if (= ~latest_ ~in)
              ~acc
              (do
                (set! ~latest_ ~in)
                ~stepform)))})

(register-xf!* (var take)
  {:state (fn [_ [n]]
            [["take-n" n {:tag 'long :volatile-mutable true}]])
   :step (fn [[n] acc _in stepform]
           `(do
              (set! ~n (dec ~n))
              (if (neg? ~n)
                (reduced ~acc)
                ~stepform)))})

(register-xf!* (var drop)
  {:state (fn [_ [n]]
            [["drop-n" n {:tag 'long :volatile-mutable true}]])
   :step (fn [[n] acc _in stepform]
           `(do
              (set! ~n (dec ~n))
              (if (nat-int? ~n)
                ~acc
                ~stepform)))})

(register-xf!* (var cat)
  {:uses-rf? true
   :state (fn [_ []]
            [["cat-subrf" nil {:volatile-mutable true}]])
   :init (fn [[subrf] rf initform]
           `(do
              (set! ~subrf (comp -preserve-reduced ~rf))
              ~initform))
   :step (fn [[subrf] _rf acc in _stepform]
           `(reduce ~subrf ~acc ~in))})

(register-xf-expander!* (var mapcat)
  (fn [f]
    `(comp
       cat
       (map ~f))))

(defn xreductions [f init]
  (fn [rf]
    (let [!state (volatile! init)]
      (fn
        ([] (vreset! !state (rf)))
        ([out]
         (vreset! !state nil)
         (rf out))
        ([acc in]
         (rf acc (vswap! !state f in)))))))

(register-xf!* (var xreductions)
  {:state (fn [_ [f init]]
            [["xreductions-f" f]
             ["xreductions-init" init {:volatile-mutable true}]])
   :step (fn [[f init] _acc in stepform]
           `(let [~in (set! ~init (~f ~init ~in))]
              ~stepform))})

(defn -cross-step
  [crossfn reducible rf acc in]
  (reduce (fn [subacc subin]
            (-preserve-reduced (rf subacc (crossfn in subin))))
    acc reducible))

(defn cross
  ([reducible]
   (cross vector reducible))
  ([crossfn reducible]
   (fn [rf]
     (fn
       ([] (rf))
       ([o] (rf o))
       ([acc in]
        (-cross-step crossfn reducible rf acc in))))))

(register-xf!* (var cross)
  {:uses-rf? true
   :state (fn [_ args]
            (apply (fn cross-state
                     ([reducible]
                      (cross-state `vector reducible))
                     ([crossfn reducible]
                      [["cross-fn" crossfn]
                       ["cross-reducible" reducible]]))
              args))
   :step (fn [[crossfn reducible] rf acc in _stepform]
           `(-cross-step ~crossfn ~reducible ~rf ~acc ~in))})

(defn cartesian
  [redu1 & reducibles]
  (apply comp
    (cross vector redu1)
    (map #(cross conj %) reducibles)))

(register-xf-expander!* (var cartesian) 
  (fn [redu1 & reducibles]
    `(comp
       (cross ~redu1)
       ~@(for [r reducibles]
           `(cross conj ~r)))))

(defn lag
  [f]
  (fn [rf]
    (let [!prev (volatile! :clojure.core/none)]
      (fn
        ([] (rf))
        ([out] (rf out))
        ([acc in]
         (let [prev @!prev]
           (vreset! !prev in)
           (if (= :clojure.core/none prev)
             acc
             (rf acc (f prev in)))))))))

(register-xf!* (var lag)
  {:state (fn [_ [f]]
            [["lag-f" f]
             ["lag-last" :clojure.core/none {:volatile-mutable true}]])
   :step (fn [[f last] acc in stepform]
           `(let [last# ~last]
              (set! ~last ~in)
              (if (= :clojure.core/none last#)
                ~acc
                (let [~in (~f last# ~in)]
                  ~stepform))))})

(comment
  (into [] (lag +) [1 2])

  (into [] (cross * [1 2 3])
    [1 2 3])

  (into [] (xreductions + 0)
    [1 2 3 4 5])

  (into [] (cartesian [1 2 3] [1 2 3])
    [1 2 3])

  (macroexpand-1
    '(xf
       (tree #(not (number? %)))
       (lag max)
       (map inc)
       (take 3)
       (lag +)
       (cartesian [1 2 3] [4 5])
       cat))

  (require '[criterium.core :as criterium])

  ;; actually a fairly generous comparison for `comp`

  (criterium/quick-bench
    (transduce (xf
                 (tree #(not (number? %)))
                 (lag max)
                 (map inc)
                 (lag +)
                 (cross [1 2 3])
                 (map #(* (% 0) (% 1))))
      + [1 2 3 4 5 6]))
  
  (criterium/quick-bench
    (transduce (comp
                 (tree #(not (number? %)))
                 (lag max)
                 (map inc)
                 (lag +)
                 (cross [1 2 3])
                 (map #(* (% 0) (% 1))))
      + [1 2 3 4 5 6]))

  *e)
