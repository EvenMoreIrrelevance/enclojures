(ns emi.enclojures.format)

(def -marker-re #"(?<![$])[$][\{](.*?)[\}]")

(comment
  (re-seq -marker-re "$${a} ${b}")
  *e)

(defmacro interp-block
  [syntax]
  (if-let [invalid-seq (re-find #"(?<![$])[$][^$\{]" syntax)]
    (throw (ex-info (str "found invalid special sequence: " invalid-seq) {}))
    `(str "" ~@(mapv #(read-string (second %)) (re-seq -marker-re syntax)))))
