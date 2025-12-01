(ns emi.enclojures.format
  (:require
   [clojure.string :as str]
   [emi.enclojures.conditions :as conditions]
   [emi.enclojures.staticenv :as staticenv :refer [nloop nrecur]]))

(defn -blank-line-start-re
  ([whitespace]
   (re-pattern (str "^(" whitespace "+).+"))))

(conditions/defcondition ::ambiguous-whitespaces ::conditions/input-error)
(defn -block-cutoff
  ([lines]
   (-block-cutoff #"\s" lines))
  ([whitespace-re lines]
   (let [all-blanks (into #{} (comp
                                (remove str/blank?)
                                (map (partial re-find (-blank-line-start-re whitespace-re)))
                                (keep second))
                      lines)
         cutoff (when (seq all-blanks) (apply min (map count all-blanks)))
         clippings (into #{} (comp
                               (remove str/blank?)
                               (map #(subs % 0 cutoff)))
                     lines)]
     (if (> (count clippings) 1)
       (conditions/raise ::ambiguous-whitespaces
         {::multiple-clippings
          clippings
          ::conditions/restarts
          {::use-spaces #(-block-cutoff #" " lines)
           ::use-tabs #(-block-cutoff #"\t" lines)
           ::conditions/ignore (constantly cutoff)}})
       cutoff))))

(comment
  (-block-cutoff [" a" "  b"])
  (try (-block-cutoff ["  a" " \tb"])
    (catch Exception e e))
  (conditions/handling [(::ambiguous-whitespaces
                         c
                         (conditions/restart-with c ::use-spaces))]
    (-block-cutoff ["  a" " \tb"]))
  *e)

(conditions/defcondition ::multiline-str-block-with-nonblank-start ::conditions/input-error)
(defn -expand-block [block-strn]
  (let [lines (str/split block-strn #"\n" -1)]
    (cond
      (= 1 (count lines))
      block-strn

      (not (str/blank? (first lines)))
      (conditions/raise ::multiline-str-block-with-nonblank-start
        {::lines lines
         ::conditions/restarts
         {::keep-line #(str (first lines) "\n" (-expand-block (str/join "\n" (cons "" (next lines)))))
          ::ignore-line #(-expand-block (cons "" (next lines)))}})

      :else
      (let [cutoff (-block-cutoff (next lines))]
        (str/join "\n"
          (map #(if (str/blank? %)
                  ""
                  (subs % cutoff))
            (next lines)))))))

(comment
  (str/split "
              foo
              bar
              
              "
    #"\n" -1)
  (-expand-block "
                  foo
                  bar
                  ")
  (conditions/handling [(::multiline-str-block-with-nonblank-start
                         c
                         (conditions/restart-with c ::keep-line))]
    (-expand-block "foo
                  bar
                  baz
                    ")))

(defmacro block
  [strn]
  (-expand-block strn))

(comment
  (block "
          Hello, I'm a
          block
            with some indentation"))

(conditions/defcondition ::interp-not-parsable)
(defn -interp
  [syntax]
  (let [error (fn [state kind to-parse]
                {:state state kind true :at (- (count syntax) (count to-parse)) :while-parsing to-parse})]
    (nloop [to-parse syntax
            state :str
            errors []
            out []]
      (case state
        :end
        (if (not-empty errors)
          (conditions/raise ::interp-not-parsable
            {:syntax syntax :errors errors :best-effort out})
          out)
        :clj
        (let [[obj consumed :as maybe-read]
              (try (read+string (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. to-parse)))
                (catch Exception _ nil))]
          (nrecur state :after-clj
            to-parse (subs to-parse (count consumed))
            errors (cond-> errors (not maybe-read) (conj (error state :failed-parsing-clj? to-parse)))
            out (conj out obj)))
        :at-special-marker
        (case (nth to-parse 0 nil)
          \$ (nrecur state :str
               out (conj out "$")
               to-parse (subs to-parse 1))
          \{ (nrecur state :clj
               to-parse (subs to-parse (count (re-find #"^[{]\s*" to-parse))))
          (nrecur state :str
            errors (conj errors (error state :bad-special-marker? to-parse))))
        :after-clj
        (if-let [to-skip (re-find #"^\s*[}]" to-parse)]
          (nrecur state :str
            to-parse (subs to-parse (count to-skip)))
          (nrecur state :str
            to-parse to-parse
            errors (conj errors (error state :unterminated-interp-marker? to-parse))))
        :str
        (if-let [dollar-ix (str/index-of to-parse "$")]
          (nrecur state :at-special-marker
            to-parse (subs to-parse (inc dollar-ix))
            out (conj out (subs to-parse 0 dollar-ix)))
          (nrecur state :end
            out (conj out to-parse)))))))

(defmacro interp-block
  [strn]
  (let [block (-expand-block strn)]
    `(str ~@(-interp block))))

(comment
  (let [the-same-kind-of-stand-as-star-platinum "Za Warudo!"]
    (interp-block "hello ${the-same-kind-of-stand-as-star-platinum}"))
  (interp-block "
                 dolla dolla 
                 $$ $$ $$ ${
                 :monies   } cash ston
                   ks")

  (let [!counter (volatile! 0)]
    (interp-block "I sure do hope ordering stays left to right! ${(vswap! !counter inc)} ${(vswap! !counter inc)}"))

  (try
    (eval '(interp-block "I fail $ because of a bad special marker"))
    (catch Exception e e)) 
  (try 
    (eval '(interp-block "I fail ${because of} trailing marker content"))
    (catch Exception e e))

  *e)
