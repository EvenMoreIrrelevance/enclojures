(ns emi.enclojures.conditions)

(defonce !condition-hierarchy (make-hierarchy))
(def ^:dynamic *-handlers*
  (conj ()
    (fn [hier cond data]
      (if (isa? hier cond ::condition)
        (throw (ex-info "unhandled condition" {:condition cond :data data}))
        (throw (ex-info "not a condition" {:condition cond :data data}))))))

(defn condition?
  [chier self]
  (isa? chier self ::condition))

(defn defcondition
  ([defined]
   (defcondition defined ::condition))
  ([defined parent]
   (alter-var-root (var !condition-hierarchy)
     #(if-not (condition? % parent)
        (throw (ex-info "can't derive from something that isn't a condition"
                 {:defined defined :parent parent}))
        (derive % defined parent)))))

(defn -toposort [graph xs]
  (let [xs (set xs)]
    (loop [stack (into () xs)
           color {}
           out []]
      (if (empty? stack)
        out 
        (let [curr (peek stack)]
          (case (color curr :white)
            :white
            (recur
              (into stack (filter #(if (= :gray (color %))
                                     (throw (ex-info "cycle in graph" {:hinge %}))
                                     (contains? xs %)))
                (graph curr))
              (assoc color curr :gray)
              out)

            :black
            (recur (pop stack) color out)

            :gray
            (recur (pop stack) (assoc color curr :black) (conj out curr))))))))

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

(defn -find-handler
  [!inline-cache hierarchy handlers condition]
  (let [cached @!inline-cache
        {:keys [handlers-by-specificity]}
        (if (identical? (:hier cached) hierarchy)
          cached
          (let [new_ {:hier hierarchy :proj (-toposort (:descendants hierarchy) handlers)}]
            (alter-var-root !inline-cache
              (fn [_] new_))))]
    (some #(when (isa? condition %) %) handlers-by-specificity)))

(defn -emit-handler 
  [bodies]
  (let [cache (intern (create-ns 'emi.enclojures.conditions.caches) (symbol (str "cache" (random-uuid))))
        handled (into #{} (map first) bodies)
        data_ (gensym "data")]
    `(fn [hier# cond# ~data_]
       (case (-find-handler ~cache hier# ~handled cond#)
         ~@(apply concat
             (for [[tag bind & body] bodies] 
               `[~tag (reduced (let [~bind ~data_] ~@body))]))
         nil))))

(comment
  (-emit-handler  
    '[[::lol _ 3]
      [::lmao {:keys [x y]} (+ x y)]])
  )

(defn -try-if-finally
  [body]
  (let [end (last body)]
    (if (= 'finally (when (seq? end) (first end)))
      `(try (do ~@(butlast body)) ~end)
      body)))

(comment
  (-try-if-finally `[1 (finally 3)])
  (-try-if-finally `[1 2])
  )

(defmacro handling [hspec & body]
  `(binding [*-handlers* (conj *-handlers* ~(-emit-handler hspec))]
     ~@(-try-if-finally body)))

(defn raise
  ([condition] (raise condition nil))
  ([condition data]
   (let [hierarchy !condition-hierarchy]
     @(some #(% hierarchy condition data) *-handlers*))))

(comment
  (do
    (defcondition ::error ::condition)
    (defcondition ::out-of-memory ::error)
    (defcondition ::minor-error ::error)
    (defcondition ::warning ::condition))
  
  (handling [(::error _ "error")
             (::minor-error e ["small error" e])
             (::warning e (do (println "warning") e))]
    [(raise ::error nil)
     (raise ::warning "hello")
     (raise ::minor-error 3)
     (try (raise ::out-of-memory) (catch Exception e (ex-message e)))
     (try (raise ::not-a-condition) (catch Exception e (ex-message e)))])
  *e)
