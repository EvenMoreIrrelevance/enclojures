(ns emi.enclojures.conditions)

(defmacro handling [spec & body]
  (let [e_ (gensym)]
    `(try (do ~@body)
       (catch Exception ~e_
         (case (identity ~e_)
           ~@(apply concat
               (for [[k bind & body] spec]
                 `[~k (let [~bind (identity ~e_)] ~@body)])))))))