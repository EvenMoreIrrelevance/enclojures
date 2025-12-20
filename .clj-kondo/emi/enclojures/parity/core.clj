(ns emi.enclojures.parity.core)

#_{:clj-kondo/ignore [:redefined-var]}
(defn update-vals 
  [m vf]
  (with-meta
    (into {} (map (fn [[k v]] [k (vf v)])) m)
    (meta m)))

(defn -delay* [f]
  (let [p (promise)]
    (reify
      clojure.lang.IDeref
      (deref [_]
        (when-not (realized? p)
          (deliver p (f)))
        @p))))

#_{:clj-kondo/ignore [:redefined-var]}
(defmacro delay
  [& body]
  `(-delay* (fn [] ~@body)))
