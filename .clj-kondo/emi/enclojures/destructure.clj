(ns emi.enclojures.destructure
  (:require [clojure.walk :as walk]
            [emi.enclojures.parity.core :refer [delay update-vals]]
            [emi.enclojures.conditions :as conditions]
            [emi.enclojures.staticenv :refer [nloop nrecur]]))

(defmacro -returning
  {:clj-kondo/lint-as 'clojure.core/let}
  [[bind val] & body]
  `(let [val# ~val, ~bind val#]
     ~@body
     val#))

(defn -index-of [xs frm]
  (loop [xs xs i 0]
    (cond
      (= frm (first xs)) i
      (next xs) (recur (inc i) (next xs)))))

(defn -binding-name [form]
  (condp #(%1 %2) form
    simple-symbol? form
    map? (:as form)
    vector?
    (if (and
          (= 2 (count form))
          (= `& (first form)))
      (recur (second form))
      (let [as-pos (-index-of form :as)]
        (when (nat-int? as-pos)
          (nth form (inc as-pos)))))))

(conditions/defcondition ::coercions-inside-named-arg ::conditions/warning)
(conditions/defcondition ::unpaired-bindings ::conditions/input-error)
(conditions/defcondition ::multiple-coercions-of-same-symbol ::conditions/warning)

(defn -partition-bindings [bindings]
  (if (empty? bindings)
    [[] []]
    (let [[_lefts rights :as out] (apply map vector (partition-all 2 bindings))]
      (if-not rights
        (conditions/raise ::unpaired-bindings
          {:bindings bindings
           ::conditions/restarts
           {::conditions/ignore #(-partition-bindings (concat bindings [nil]))}})
        out))))

(comment
  (conditions/handling [(::unpaired-bindings c (conditions/restart-with c ::conditions/ignore))]
    (-partition-bindings [1 2 3])))

(defn -destructure-with-coercelist
  [dsform]
  (let [!binds (volatile! [])
        dsform-without-coercions
        (walk/prewalk (fn [subform]
                        (if (not (seq? subform))
                          subform
                          (let [[h s & m] subform
                                bn (-binding-name s)
                                mid (or bn (gensym))
                                {subform-without-coercions :without-coercions :keys [coercions]}
                                (-destructure-with-coercelist s)]
                            (vswap! !binds conj `[~subform-without-coercions (~h ~mid ~@m)])
                            (when (seq coercions)
                              (when bn
                                (conditions/raise ::coercions-inside-named-arg
                                  {::conditions/message "NB: named args with coercions inside are bound to their uncoerced form."}))
                              (vswap! !binds into coercions))
                            mid)))
          dsform)
        coercions
        @!binds]
    (when-let [multiple-coercions (not-empty (into {} (remove #(= 1 (count (val %))))
                                               (update-vals (group-by first coercions)
                                                 #(mapv second %))))]
      (conditions/raise ::multiple-coercions-of-same-symbol
        {::conditions/message
         (str "Multiple coercions of the same symbol in coerce-list destructuring: " multiple-coercions)}))
    {:without-coercions dsform-without-coercions
     :coercions coercions}))

(comment
  (-destructure-with-coercelist
    '{:keys [a (hash-map {:as [b (int d)]}) (int c (float a)) (int c)]})
  *e)

(defn -c-for-let-like
  [head form]
  (let [[binds & body] (next form)
        [lefts rights] (-partition-bindings binds)
        {lefts' :without-coercions :keys [coercions]} (-destructure-with-coercelist lefts)]
    `(~(with-meta head (meta (first form)))
      [~@(interleave lefts' rights)
       ~@(apply concat coercions)]
      ~@body)))

(defmacro letc [& _let-syntax]
  (-c-for-let-like `let &form))

(defmacro loopc [& _loop-syntax]
  (-c-for-let-like `loop &form))

;;; and now for a party trick
(defmacro nloopc [& _nloop-syntax]
  (-c-for-let-like `nloop &form))

(defn -arities-with-coercions
  [arities]
  (for [[argv & body] arities]
    (let [{argv' :without-coercions :keys [coercions]} (-destructure-with-coercelist argv)]
      `(~(with-meta argv' (meta argv))
        (let [~@(apply concat coercions)]
          ~@body)))))

(defn -parse-fn-like [syntax]
  (let [[prologue body] (split-with (complement sequential?) syntax)
        multi-arity? (seq? (first body))
        arities (if multi-arity? (take-while seq? body) [body])
        epilogue (when multi-arity? (drop-while seq? body))]
    {:prologue prologue
     :arities arities
     :epilogue epilogue}))

(defmacro defnc
  [name & defn-syntax]
  (let [head (with-meta `clojure.core/defn (meta (first &form)))
        {:keys [prologue arities epilogue]} (-parse-fn-like defn-syntax)
        arglists (delay `(quote ~(map first arities)))]
    `(~head ~name ~@(for [e prologue]
                      (if-not (map? e)
                        e
                        (update e :arglists #(or % @arglists))))
            ~@(when-not (some map? prologue) [{:arglists @arglists}])
            ~@(-arities-with-coercions arities)
            ~@epilogue)))

(defn -c-for-fn [head form]
  (let [{:keys [prologue arities epilogue]} (-parse-fn-like (next form))]
    `(~(with-meta head (meta (first form)))
      ~@prologue
      ~@(-arities-with-coercions arities)
      ~@epilogue)))

(defmacro fnc [& _] (-c-for-fn `fn &form))
(defmacro bound-fnc [& _] (-c-for-fn `bound-fn &form))

(comment
  (letc [(parse-long a) "3"] a)

  (macroexpand-1 '(defnc foo [(int a)] a))

  (defnc foo2
    ([(int a)] a)
    ([(int a) b] [a b]))

  ((fnc bar ([(parse-long a)] a)) "3")

  (defn args-map [args]
    (nloopc [m nil
             rem args]
      (if-not (next rem)
        (or
          (cond->> (first rem) m (merge m))
          {})
        (nrecur
          m (assoc m (first rem) (second rem))
          rem (nnext rem)))))

  (defnc frob [foo & (args-map {(parse-long a) :b :as args})]
    [foo args a])

  (frob 1 {:b "4"})

  *e)
