(ns emi.enclojures.xfs
  (:require [emi.enclojures.conditions :as conditions]
            [emi.enclojures.destructure :refer [defnc]]))

(defn -fixpoint
  [eq f val]
  (let [val' (f val)]
    (if (eq val' val)
      val'
      (recur eq f val'))))

(defn -ensure-reduced [x]
  (if (reduced? x)
    (reduced x)
    x))

(defn -sresolve [in]
  (cond
    (or (class? in) (var? in)) in
    (symbol? in) (resolve in)
    :else nil))

(comment
  `(xf
     (keep set?)
     (mapcat inc)
     (filter odd?)
     (take 3))

  (into [] (comp
             (filter set?)
             cat
             (map inc)
             (filter odd?)
             (take 3))
    [#{1 2 3 4 5 6} [4 5 6] #{1 2 3 4 5 6}])


  `(xf
     (keep set?)
     cat
     (map inc)
     (filter odd?)
     (take 3))

  `(eval
     (deftype _Xf2_0 [rf keep-fn-69]
       (invoke [_]
         (rf))
       (invoke [_ out]
         (rf out))
       (invoke [_ acc in]
         (if-some [in (keep-fn-69 in)]
           (rf acc in)
           acc)))

     (deftype _Xf2_1 [rf
                      map-fn-69
                      filter-p-69
                      take-n-69]
       (invoke [_]
         (rf))
       (invoke [_ out]
         (rf out))
       (invoke [_ acc in]
         (let [in (map-fn-69 in)]
           (if-not (filter-p-69 in)
             acc
             (do
               (set! take-n-69 (dec take-n-69))
               (if-not (neg? take-n-69)
                 acc
                 (rf acc in)))))))

     (comp
       (fn [rf] (_Xf2_0. rf set?))
       cat
       (fn [rf] (_Xf2_1. rf inc odd? 3))))

  '(xf
     (map inc)
     (filter odd?)
     (take 3)
     (partition-all some-value))
  '(eval (deftype _Xf [rf
                       map-fn-123
                       filter-fn-123
                       ^:volatile-mutable ^long take-state-123
                       ^long partition-count-123
                       ^:volatile-mutable partition-state-123]
           clojure.lang.IFn
           (invoke [_]
             (rf))
           (invoke [_ in]
             (let [final (vec (.toArray partition-state-123))
                   out (unreduced (rf out final))]
               (rf out)))
           (invoke [_ acc in]
             (let [in (map-fn-123 in)]
               (if-not (filter-fn-123 in)
                 acc
                 (if (zero? take-state-123)
                   (reduced acc)
                   (if (= partition-count-123 (.size partition-state-123))
                     (let [in (vec (.toArray partition-state-123))]
                       (.clear partition-state-123)
                       (rf acc in))
                     (do (.add partition-state-123 in)
                       acc)))))))
     `(fn [rf] (_xf. rf inc odd? 3 some-value (java.util.ArrayList.))))
  '(fn [rf]
     (fn
       ([] (rf)))))

(conditions/defcondition ::unregistered-xf ::conditions/error)

(defn -var-or-default [xf]
  (let [var?? (-sresolve xf)]
    (if (var? var??)
      var??
      :default)))

(defmulti xf-state (fn [xf _args] (-var-or-default xf)))
(defmulti xf-init (fn [xf _state _initform] (-var-or-default xf)))
(defmulti xf-step (fn [xf _state _acc _in _stepform] (-var-or-default xf)))
(defmulti xf-finalize (fn [xf _state _out _finalform] (-var-or-default xf)))
(defmulti xf-expand (fn [[xf & _args]] (-var-or-default xf)))

(defmethod xf-expand :default [frm] frm)

(defmethod xf-state :default
  [xf args]
  (conditions/raise ::unregistered-xf
    {:xf xf
     ::conditions/restarts
     {::conditions/retry #(xf-state xf args)}}))

(defn -default-state
  [xf args]
  (mapv vector
    (map #(str (name xf) "-arg-" % "-") (range))
    args))
(defn -default-init [_state initform] initform)
(defn -default-finalize [_state _out finalform] finalform)

(conditions/defcondition ::xf-step-not-registered ::conditions/input-error)

(defnc register-xf!*
  [thevar {:keys [(or state -default-state)
                  (or init -default-init)
                  (or finalize -default-finalize)
                  (or step (conditions/raise ::xf-step-not-registered))]}]
  (defmethod xf-state thevar [xf args]
    (state xf args))
  (defmethod xf-init thevar [_xf state initform]
    (init state initform))
  (defmethod xf-finalize thevar [_xf state out finalform]
    (finalize state out finalform))
  (defmethod xf-step thevar [_xf state acc in stepform]
    (step state acc in stepform))
  {:state state
   :init init
   :finalize finalize
   :step step})

(defn register-xf-expander!*
  [thevar fun]
  (defmethod xf-expand thevar [args]
    (apply fun args)))

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

(defn cross
  ([reducible]
   (cross vector reducible))
  ([crossfn reducible]
   (fn [rf]
     (fn
       ([] (rf))
       ([o] (rf o))
       ([acc in]
        (reduce (fn [subacc subin]
                  (-ensure-reduced (rf subacc (crossfn in subin))))
          acc reducible))))))

(defn cartesian
  [redu1 & reducibles]
  (apply comp
    (cross vector redu1)
    (map #(cross conj %) reducibles)))

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
  
  *e)

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
  (fn remove-expander [_ f]
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
              (if (neg? ~n)
                ~stepform
                ~acc)))})

(register-xf-expander!* (var mapcat)
  (fn [_ f]
    `(comp
       cat
       (map ~f))))

(defn -tree-rf-step
  ([rf node? children]
   (fn tree-rf-step [acc in]
     (-ensure-reduced
       (if (node? in)
         (reduce tree-rf-step acc (children in))
         (rf acc in))))))

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
                   (tree
                     #(and (seq? %) (see-through? (-sresolve (first %))))
                     next)
                   (map #(if (seq? %)
                           (-fixpoint identical? xf-expand %)
                           %)))
          args))
      args)))

(defn -ensure-sequential [x]
  (if-not (sequential? x)
    (list x)
    x))

(defn -xf-inlinable
  [expr]
  (if (contains? (methods xf-state)
        (-sresolve (first (-ensure-sequential expr))))
    ::inlinable
    (Object.)))

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
          states (mapv (fn [[h & args]]
                         (xf-state h args)) xfs)
          init-lists (mapv (fn [specs]
                             (mapv (fn [[h _v m]]
                                     (with-meta (gensym (symbol h)) m))
                               specs))
                       states)
          steps (rseq (mapv vector (map first xfs) init-lists))
          acc_ (gensym "acc")
          in_ (gensym "in")
          out_ (gensym "out")
          rf_ (gensym "rf")
          self_ `_#
          typname (with-meta (symbol (str "_Xf" (random-uuid))) {:private true})]
      {:type
       `(deftype ~typname [~rf_ ~@(apply concat init-lists)]
          clojure.lang.IFn
          (~'invoke
           [~self_]
           ~(reduce (fn [initform [h state]] (xf-init h state initform))
              `(~rf_) steps))
          (~'invoke
           [~self_ ~out_]
           ~(reduce (fn [finalform [h state]] (xf-finalize h state out_ finalform))
              `(~rf_ ~out_) steps))
          (~'invoke
           [~self_ ~acc_ ~in_]
           ~(reduce (fn [stepform [h state]] (xf-step h state acc_ in_ stepform))
              `(~rf_ ~acc_ ~in_) steps))
          (~'applyTo
           [~self_ seq#]
           (clojure.lang.AFn/applyToHelper ~self_ seq#)))
       :fn
       `(let [~@bindings]
          (fn [rf#] (new ~typname rf# ~@(map (fn [[_h v _m]] v) (apply concat states)))))})))

(defn -xf* [& args]
  (let [types-and-fns
        (into [] (comp
                   (partition-by -xf-inlinable)
                   (map -xf-chunk->type-and-fn))
          (-xf-chain args))]
    (run! eval (keep :type types-and-fns))
    `(comp ~@(map :fn types-and-fns))))

(comment
  (xf-expand `(remove odd?))

  (into [] (comp
             (partition-by even?)
             (map vec))
    [1 2 2 3 3 3 4])
  
  (into [] (comp
             (partition-by -xf-inlinable)
             (map -xf-chunk->type-and-fn))
    (-xf-chain
      '[(mapcat dec)
        (remove odd?)
        (map inc)
        (drop 9)
        (dedupe)
        (comp
          cat
          cat
          (keep 5))]))
  
  (mapv -xf-chunk->type-and-fn
    (partition-by -xf-inlinable
      (-xf-chain
        '[(mapcat dec)
          (remove odd?)
          (map inc)
          (drop 9)
          (dedupe)
          (comp
            cat
            cat
            (keep 5))]))) 

  (methods xf-state)

  (apply -xf*
    '[(mapcat dec)
      (remove odd?)
      (map inc)
      (drop 9)
      (dedupe)
      (comp
        cat
        cat
        (keep odd?))])
  *e)

(defmacro xf [& args]
  (apply -xf* args))

(comment
  (into [] (xf
             (tree #(not (number? %)))
             (lag max)
             (map inc)
             (take 3))
    [[1 2 3] 4 [[5 6]]])
  *e)
