(ns emi.enclojures.staticenv)

(def -entry-point 'EMI_enclojures_staticenv_entrypoint)
(defn static-env*
  [env]
  (let [[k] (find env -entry-point)]
    (::static-env (meta k))))

(defmacro static-env 
  []
  `(static-env* ~'&env))

(defn with-static-binding* 
  [base-env keys->vals & body]
  `(let [~(with-meta -entry-point {::static-env (merge (static-env* base-env) keys->vals)}) nil]
     ~@body))
(defmacro with-static-binding 
  [keys->vals & body]
  (apply with-static-binding* &env keys->vals body))

(defmacro nloop
  {:clj-kondo/lint-as 'clojure.core/loop}
  [loop-head & body]
  `(with-static-binding ~{::nloop-keys (mapv first (partition 2 loop-head))}
     (loop ~loop-head 
       ~@body)))

(defmacro nrecur
  [& {:as names->vals}]
  (let [{::keys [nloop-keys]} (static-env)
        extraneous (keys (apply dissoc names->vals nloop-keys))]
    (if (not-empty extraneous)
      (throw (ex-info "no such loop variables" {:extraneous extraneous}))
      `(recur ~@(for [k nloop-keys] (get names->vals k k))))))

(defn -toposort [graph xs]
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
