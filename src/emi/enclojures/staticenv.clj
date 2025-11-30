(ns emi.enclojures.staticenv
  (:require [emi.enclojures.conditions :as conditions]))

(def -entry-point 'EMI_enclojures_staticenv_entrypoint)
(defn static-env*
  [env]
  (let [[k] (find env -entry-point)]
    (::static-env (meta k))))

(defmacro static-env
  []
  `(static-env* ~'&env))

(defn with-static-env*
  [base-env keys->vals & body]
  `(let [~(with-meta -entry-point {::static-env (merge (static-env* base-env) keys->vals)}) nil]
     ~@body))

(defmacro with-static-env
  [keys->vals & body]
  (apply with-static-env* &env keys->vals body))

(defn -binding-name [form]
  (condp #(%1 %2) form
    simple-symbol? form
    map? (:as form)
    vector?
    (if (and
          (= 2 (count form))
          (= `& (first form)))
      (recur (second form))
      (let [as-pos (.indexOf ^java.util.List form :as)]
        (when (nat-int? as-pos)
          (nth form (inc as-pos)))))))

(conditions/defcondition ::unresolved-loop-bindings ::conditions/input-error)
(defmacro nloop
  [loop-head & body]
  (let [bindings (mapv first (partition 2 loop-head))
        binding-names (mapv -binding-name bindings)]
    (or
      (when (some nil? binding-names)
        (conditions/raise ::unresolved-loop-bindings
          {:bindings bindings :binding-names binding-names
           ::conditions/restarts {::conditions/ignore (constantly nil)}}))
      `(with-static-env ~{::nloop-var-names binding-names}
         (loop ~loop-head
           ~@body)))))

(conditions/defcondition ::no-such-loop-variable ::conditions/input-error)
(defmacro nrecur
  [& {:as names->vals}]
  (let [{::keys [nloop-var-names]} (static-env)
        extraneous (vec (keys (apply dissoc names->vals nloop-var-names)))]
    (or
      (when (seq extraneous)
        (conditions/raise ::no-such-loop-variable
          {:extraneous extraneous
           ::conditions/restarts {::conditions/ignore (constantly nil)}}))
      `(recur ~@(for [k nloop-var-names] (get names->vals k k))))))

(defn -toposort
  [graph xs]
  (let [xs (set xs)]
    (nloop [stack (into () xs)
            color {}
            out []]
      (if (empty? stack)
        out
        (let [curr (peek stack)]
          (case (color curr :white)
            :white
            (nrecur
              stack (into stack (filter
                                  #(if (= :gray (color %))
                                     (throw (ex-info "cycle in graph" {:hinge %}))
                                     (contains? xs %)))
                      (graph curr))
              color (assoc color curr :gray))

            :black
            (nrecur stack (pop stack))

            :gray
            (nrecur
              stack (pop stack)
              color (assoc color curr :black)
              out (conj out curr))))))))

(comment
  (-toposort
    {:a [:b :c :d]
     :b [:c :d]
     :d [:e]}
    [:a :b :d :c :e])
  (try
    (-toposort
      {:a [:b :c :d]
       :b [:c :d]
       :d [:e :a]}
      [:a :b :c :d :e])
    (catch Exception e
      e))
  *e)
