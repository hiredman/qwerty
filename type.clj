(defmulti type-string type)
(defmulti type-string-seq first)
(defmethod type-string clojure.lang.Symbol [s]
  (if (= 'interface s)
    "interface{}"
    (let [n (name s)]
      (if (.startsWith n (str (name *package*) "."))
        (.replaceFirst n (str (name *package*) ".") "")
        n))))
(defmethod type-string clojure.lang.ISeq [s]
  (type-string-seq s))
(defmethod type-string String [s]
  s)
(defmethod type-string-seq '* [[_ v]]
  (str "*" (type-string v)))
(defmethod type-string-seq 'map [[_ key value]]
  (str "map[" (type-string key) "]" (type-string value)))
(defmethod type-string-seq 'slice [[_ type]]
  (str "[]" (type-string type)))
(defmethod type-string-seq 'chan [[_ type]]
  (str "chan " (type-string type)))
