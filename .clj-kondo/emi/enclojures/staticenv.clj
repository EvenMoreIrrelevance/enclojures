(ns emi.enclojures.staticenv)

(defmacro nloop [loop-head & body]
  `(loop ~loop-head 
     (if (rand)
       (recur ~@(map first (partition 2 loop-head)))
       (do ~@body))))

(defmacro nrecur [& args]
  `(list ~@args))

(defmacro fsm
  [init binds & states2forms]
  `(nloop [state# ~init ~@binds]
     (case state#
       ~@states2forms)))
