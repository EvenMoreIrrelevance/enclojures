(ns emi.enclojures.conditions)

(def -set-stack-trace (memfn ^Exception setStackTrace st))

(defonce !condition-hierarchy (make-hierarchy))
(declare raise)
(def ^:dynamic *-handlers*
  (conj ()
    (fn [hier cond_ data]
      (cond
        (isa? hier cond_ ::not-a-condition)
        (throw (ex-info "not a condition" data))
        (isa? hier cond_ ::warning)
        (binding [*out* *err*]
          (println (str "caught warning: " cond_ "\n" (::message data)))
          (reduced nil))
        (isa? hier cond_ ::condition)
        (throw (cond-> (ex-info
                         (str "unhandled condition: " cond_ "\n" (::message data))
                         (dissoc data ::stacktrace))
                 (::stacktrace data) (doto (-set-stack-trace (::stacktrace data)))))
        :else
        (reduced (raise ::not-a-condition {:condition cond_ :data data}))))))

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

(defn undef-condition
  [condition]
  (alter-var-root (var !condition-hierarchy)
    (fn [h]
      (or
        (reduce #(underive % condition %2) h (parents h condition))
        (make-hierarchy)))))

(defn -throw-if-gray [color node]
  (if (= :gray (color node))
    (throw (ex-info "cycle in graph" {:hinge node}))
    node))

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
              (into stack (comp
                            (filter (partial contains? xs))
                            (map (partial -throw-if-gray color)))
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
        {:keys [hier handlers-by-specificity]}
        (if (identical? (:hier cached) hierarchy)
          cached
          (let [new_ {:hier hierarchy :handlers-by-specificity (-toposort (:descendants hierarchy) handlers)}]
            (alter-var-root !inline-cache
              (fn [_] new_))))]
    (some #(when (isa? hier condition %) %) handlers-by-specificity)))

(defn -reduced-unless-pass
  [result]
  (when-not (= result ::pass)
    (reduced result)))

(defn -emit-handler
  [bodies]
  (let [cache (intern
                (create-ns 'emi.enclojures.conditions.caches)
                (gensym)
                nil)
        handled (into #{} (map first) bodies)
        data_ (gensym "data")]
    (or
      (when-let [non-conditions (not-empty
                                  (into #{} (comp
                                              (map first)
                                              (remove (partial condition? !condition-hierarchy)))
                                    bodies))]
        (raise ::handler-for-noncondition
          {::message (str "emitting handlers for non-conditions: " non-conditions)}))
      `(fn [hier# cond# ~data_]
         (case (-find-handler ~cache hier# ~handled cond#)
           ~@(apply concat
               (for [[tag bind & body] bodies]
                 `[~tag (-reduced-unless-pass (let [~bind ~data_] ~@body))]))
           nil)))))

(comment
  (-emit-handler
    '[[::lol _ 3]
      [::lmao {:keys [x y]} (+ x y)]]))

(defn -try-if-finally
  [body]
  (let [end (last body)]
    (if (= 'finally (when (seq? end) (first end)))
      `(try (do ~@(butlast body)) ~end)
      body)))

(comment
  (-try-if-finally `[1 (finally 3)])
  (-try-if-finally `[1 2]))

(defmacro handling [hspec & body]
  `(binding [*-handlers* (conj *-handlers* ~(-emit-handler hspec))]
     ~@(-try-if-finally body)))

(defn -coerce-condition
  [condition data]
  (if-not (map? data)
    {::data data ::handling condition}
    (try (assoc data ::handling condition)
      (catch Exception e
        {::data data ::handling condition ::error-associating e}))))

(defn -raise-coerced [condition data]
  (let [hierarchy !condition-hierarchy]
    @(some #(% hierarchy condition data) *-handlers*)))

(defn raise-without-stacktrace
  ([condition]
   (raise-without-stacktrace condition nil))
  ([condition data]
   (-raise-coerced condition (-coerce-condition condition data))))

(defn -with-stacktrace [map] map)

(defn raise
  ([condition]
   (raise condition nil))
  ([condition data]
   (-raise-coerced condition (-with-stacktrace (-coerce-condition condition data)))))

(defn restart-with [cond_ restart-name & args]
  (if-let [restart-fn (get (::restarts cond_) restart-name)]
    (apply restart-fn args)
    (raise ::no-such-restart {::cause cond_ :restart restart-name})))

(defcondition ::error)
(defcondition ::input-error ::error)
(defcondition ::no-such-restart ::error)
(defcondition ::warning)
(defcondition ::not-a-condition)
(defcondition ::handler-for-noncondition ::warning)

(comment
  (do
    (defcondition ::out-of-memory ::error)
    (defcondition ::minor-error ::error))

  (handling [(::error _ "error")
             (::minor-error e ["small error" e])
             (::warning e (do (println "warning") e))
             ("couldn't be a condition if it tried" _ 1)]
    [(raise ::error nil)
     (raise ::warning "hello")
     (raise ::minor-error 3)
     (try (raise ::out-of-memory) (catch Exception e (ex-message e)))
     (try (raise ::not-a-condition) (catch Exception e (ex-message e)))])
  *e)
