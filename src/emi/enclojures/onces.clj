(ns emi.enclojures.onces
  (:require
   [clojure.string :as str])
  (:import
   (clojure.asm ClassWriter Opcodes MethodWriter Type)))

(set! *warn-on-reflection* true)

(defn -handle
  (^String [^Class cls]
   (Type/getInternalName cls)))

(defn -descriptor
  (^String [^Class cls]
   (Type/getDescriptor cls)))

(defn -emit-cast [^MethodWriter m ^Class type]
  (if-not (.isPrimitive type)
    (.visitTypeInsn m Opcodes/CHECKCAST (-handle type))
    (let [box-type (class (first (make-array type 1)))]
      (doto m
        (-emit-cast box-type)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (-handle box-type)
          (str (.getName type) "Value") (str "()" (-descriptor type)) false)))))

(defn -emit-init-static-field [class-handle ^MethodWriter m {:keys [type name]}]
  (doto m
    (.visitInsn Opcodes/DUP)
    (.visitLdcInsn name)
    (.visitMethodInsn 
      Opcodes/INVOKEINTERFACE "clojure/lang/IPersistentMap" "valAt"
      "(Ljava/lang/Object;)Ljava/lang/Object;" true)
    (-emit-cast type)
    (.visitFieldInsn Opcodes/PUTSTATIC class-handle name (-descriptor type))))

(defn -emit-clinit
  [^ClassWriter c {:keys [class-handle initf-handle fields]}]
  (let [m (.visitMethod c (+ Opcodes/ACC_STATIC) "<clinit>" "()V" nil nil)]
    (doto m
      (.visitCode)
      (.visitTypeInsn Opcodes/NEW initf-handle)
      (.visitInsn Opcodes/DUP)
      (.visitMethodInsn Opcodes/INVOKESPECIAL initf-handle "<init>" "()V" false)
      (.visitMethodInsn Opcodes/INVOKEINTERFACE "clojure/lang/IFn" "invoke" "()Ljava/lang/Object;" true)
      (.visitTypeInsn Opcodes/CHECKCAST "clojure/lang/IPersistentMap"))
    (run! (partial -emit-init-static-field class-handle m) fields)
    (doto m
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

(defn -load-class!
  [name bytecode]
  (when *compile-files* (Compiler/writeClassFile (str/replace name "." "/") bytecode))
  (let [^clojure.lang.DynamicClassLoader loader @clojure.lang.Compiler/LOADER]
    (.defineClass loader name bytecode nil)))

(defn -emit-static-grabbag
  [{:keys [class-name class-handle fields] :as opts}]
  (let [c (doto (ClassWriter. (+ ClassWriter/COMPUTE_MAXS ClassWriter/COMPUTE_FRAMES))
            (.visit 52 (+ Opcodes/ACC_FINAL Opcodes/ACC_PUBLIC) class-handle nil "java/lang/Object" nil))]
    (doseq [{:keys [type name]} fields]
      (.visitField c (+ Opcodes/ACC_FINAL Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) name (-descriptor type) nil nil))
    (doto c
      (-emit-clinit opts)
      (.visitEnd))
    (-load-class! class-name (.toByteArray c))))

(defn -array-class [elt nesting]
  (when (class? elt)
    (if-not (pos? nesting)
      elt
      (recur
        (class (make-array elt 0))
        (dec nesting)))))

(defn -primitive-tag->type
  [tag-str]
  (let [elt-tag?? (second (re-matches #"(.*)s" tag-str))]
    (cond-> (case (or elt-tag?? tag-str)
              "byte" Byte/TYPE
              "short" Short/TYPE
              "int" Integer/TYPE
              "long" Long/TYPE
              "float" Float/TYPE
              "double" Double/TYPE
              "char" Character/TYPE
              "boolean" Boolean/TYPE
              nil)
      elt-tag?? (-array-class 1))))

(defn -special-tag->type
  [tag]
  (let [normalized-name?? (cond-> tag (simple-symbol? tag) (name))]
    (or
      (when (string? normalized-name??)
        (-primitive-tag->type normalized-name??))
      (when (symbol? tag)
        (when-let [n (parse-long (name tag))]
          (when (< 0 n 10)
            (-array-class (resolve (symbol (namespace tag))) n)))))))

(comment
  (mapv #(some-> % Class/.getName)
    (mapv -special-tag->type
      '[int "ints" Object/3 "cans"]))
  *e)

(defn -tag->type
  [tag]
  (or
    (-special-tag->type tag)
    (let [interpretation
          (cond
            (class? tag) tag
            (string? tag) (resolve (symbol tag))
            (symbol? tag) (resolve tag)
            :else nil)]
      (if (class? interpretation)
        interpretation
        Object))))

(defn -map-by
  ([kf xs]
   (-map-by kf identity xs))
  ([kf vf xs]
   (into {} (map (juxt kf vf)) xs)))

(defn -strict-uninterleave
  [n xs]
  (vec
    (let [xs (seq xs)]
      (if-not xs
        (repeat n [])
        (let [parts (apply mapv vector (partition-all n xs))]
          (if-not (get parts (dec n))
            (throw (ex-info "couldn't uninterleave input" {:n n :xs xs}))
            parts))))))

(defn with-statics*
  [bindings body]
  (let [cname
        (str (namespace-munge *ns*) "._EMI_Grabbag_" (str/replace (random-uuid) "-" ""))
        [fields inits]
        (-strict-uninterleave 2 bindings)
        untagged-fields
        (mapv #(vary-meta % dissoc :tag) fields)
        initf-class
        (class
          (eval
            `(fn []
               (let [~@(interleave untagged-fields inits)]
                 ~(-map-by name fields)))))
        grabbag
        (-emit-static-grabbag
          {:initf-handle (-handle initf-class)
           :class-name cname
           :class-handle (str/replace cname "." "/")
           :fields (mapv (fn [field]
                           {:type (-tag->type (:tag (meta field)))
                            :name (name (munge field))})
                     fields)})]
    `(let [~@(mapcat (fn [fd] `[~fd (. ~grabbag ~fd)]) untagged-fields)]
       ~@body)))

(defmacro with-statics
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  (with-statics* bindings body))

(defmacro static
  [& body]
  (let [s_ (with-meta (gensym) (select-keys (meta &form) [:tag]))]
    `(with-statics [~s_ (do ~@body)]
       ~s_)))

(defn lol []
  (static (println "once") 3))

(comment
  (with-statics [a "a" ^String b (str a "b") ^int c (int 3)]
    (str a b c))

  (lol)

  (binding [*compile-files* true
            *compile-path* "resources"]
    (compile 'emi.enclojures.onces))

  *e)
