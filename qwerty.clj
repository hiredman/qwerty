#!/usr/bin/java -jar /Users/hiredman/src/clojure/target/clojure-1.5.0-master-SNAPSHOT.jar

(require '[clojure.walk :as w]
         '[clojure.pprint :refer [pprint]])

(declare ^:dynamic *context*)
(declare ^:dynamic *scope*)

(defmulti lower type)

(defmulti lower-seq first)

(defmethod lower clojure.lang.Symbol [s]
  s)

(defmethod lower clojure.lang.ISeq [s]
  (lower-seq s))

(defmethod lower-seq 'qwerty/fn* [[_ args body]]
  (let [function-name (gensym 'fn)
        function-type-name (gensym 'Tfn)
        struct-name (gensym 'Sfn)
        local-name (gensym 'fn)]
    `(qwerty/do
       (qwerty/defgofun ~function-name ~args
         ((~'string) ~'string)
         (qwerty/do ~(lower body)))
       (qwerty/type ~function-type-name ((~'string)))
       (qwerty/struct ~struct-name
                      ~'_fun ~function-type-name)
       (qwerty/let* ((~local-name (qwerty/new ~struct-name ~function-name)))
                    (qwerty/do
                      ~local-name)))))

(defmethod lower-seq 'qwerty/defgofun [[_ function-name args types & body]]
  `(qwerty/defgofun ~function-name ~args ~types
     (qwerty/do ~@(map lower body))))

(defmethod lower-seq 'qwerty/. [[_ func & args]]
  `(qwerty/. ~func ~@args))

(defmethod lower-seq 'qwerty/do [[_ & body]]
  (cons 'qwerty/do (map lower body)))

(defmethod lower-seq 'qwerty/let* [[_ bindings & body]]
  `(qwerty/let* ~(for [[n v] bindings]
                   `(~n ~(lower v)))
                (qwerty/do
                  ~@(map lower body))))

(defmethod lower-seq 'qwerty/.- [expr]
  expr)

(defmethod lower-seq :default [[fun & args]]
  (assert (not= "qwerty" (namespace fun)))
  (lower `(qwerty/let* ((~'f (qwerty/.- ~fun ~'_fun))
                        (~'r (qwerty/. ~'f ~@args)))
                       ~'r)))

(defmulti go type)

(defmulti go-seq first)

(defmethod go clojure.lang.ISeq [s]
  (go-seq s))

(defmethod go java.lang.String [s]
  (print " ")
  (pr s)
  (print " "))

(defmethod go clojure.lang.Symbol [s]
  (when (not= *scope* :package)
    (if (= :return *context*)
      (print "return"))
    (print (str " " s " "))
    (if (= :return *context*)
      (println))))

(defmethod go nil [s]
  (when (not= *scope* :package)
    (if (= :return *context*)
      (print "return"))
    (print " null ")
    (if (= :return *context*)
      (println))))

(defmethod go-seq 'qwerty/do [s]
  (doseq [item (butlast (rest s))]
    (binding [*context* :statement]
      (go item)))
  (go (last s)))

(defmethod go-seq 'qwerty/defgofun [[_ fun-name args types & body]]
  (binding [*scope* :function]
    (println "func" fun-name (str "(" (apply str (interpose \, (map
                                                                (fn [arg-name type]
                                                                  (str arg-name " " type))
                                                                args (first types))))
                                  ")") (str (when (> (count types) 1)
                                              (last types))) "{")
    (binding [*context* (if (> (count types) 1)
                          :return
                          :statement)]
      (go (cons 'qwerty/do body)))
    (when-not (> (count types) 1)
      (println))
    (println "}")
    (println)))

(defmethod go-seq 'qwerty/type [[_ type-name type-expression]]
  (println "type" type-name "func(string) string")
  (println))


(defmethod go-seq 'qwerty/struct [[_ struct-name func-field func-type]]
  (println "type" struct-name "struct {")
  (println " " func-field func-type)
  (println "}")
  (println))

(defmethod go-seq 'qwerty/new [[_ struct-name & values]]
  (if (= :return *context*)
    (print "return"))
  (print "" struct-name "{")
  (doseq [v values]
    (go v))
  (print "}")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/let*  [[_ [[n v]] body]]
  (cond
   (and (seq? v)
        (= 'qwerty/let* (first v)))
   (do
     (go v)
     (println)
     (recur `(qwerty/let* ((~n ~(last v))) ~body)))
   (= *scope* :function)
   (do
     (print " " n ":=")
     (go v)
     (println)
     (go body))
   :else
   (do
     (print "var " n "=")
     (go v)
     (println)
     (go body))))

(defmethod go-seq 'qwerty/. [[_ func & args]]
  (if (= :return *context*)
    (print "return"))
  (print "" func "(")
  (doseq [v args]
    (go v)
    (print ","))
  (print ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/.- [[_ obj field]]
  (if (= :return *context*)
    (print "return"))
  (print (str " " obj "." field " "))
  (if (= :return *context*)
    (println)))

(defmulti raise-decls type)

(defmethod raise-decls :default [s] s)

(defmulti raise-decls-seq first)

(defmethod raise-decls clojure.lang.ISeq [s]
  (raise-decls-seq s))

(defn decl? [form]
  (and (seq? form)
       ('#{qwerty/defgofun
           qwerty/type
           qwerty/struct} (first form))))

(defmethod raise-decls-seq 'qwerty/defgofun [[_ function-name args types body]]
  (let [body (if (seq? body) body `(qwerty/do ~body))
        defs (doall (filter decl? body))
        body (doall (map raise-decls (remove decl? body)))]
    `(qwerty/do
       ~@defs
       (qwerty/defgofun ~function-name ~args ~types
         ~body))))

(defmethod raise-decls-seq 'qwerty/let* [[_ bindings body]]
  (let [body (if (seq? body) body `(qwerty/do ~body))
        defs (doall (filter decl? body))
        body (map raise-decls (remove decl? body))
        raised-bindings (for [[n v] bindings]
                          (if (seq? v)
                            {:n n
                             :defs (doall (filter decl? v))
                             :body (doall (map raise-decls (remove decl? v)))}
                            {:n n
                             :body v
                             :defs nil}))
        bindings (doall (for [{:keys [n body]} raised-bindings]
                          `(~n ~body)))
        defs (doall (concat defs (mapcat :defs raised-bindings)))]
    `(qwerty/do
       ~@defs
       (qwerty/let* ~bindings
                    ~body))))

(defmethod raise-decls-seq 'qwerty/do [[_ & body]]
  (cons 'qwerty/do (map raise-decls body)))

(defmethod raise-decls-seq :default [exp]
  (doall (map raise-decls exp)))

(defn collapse-do [form]
  (w/postwalk
   (fn [form]
     (if (and (seq? form)
              (= (first form) 'qwerty/do))
       (if (= 2 (count form))
         (last form)
         (mapcat
          (fn [form]
            (let [form form]
              (if (and (seq? form)
                       (= 'qwerty/do (first form)))
                (rest form)
                [form])))
          form))
       form))
   form))

(defn split-lets [form]
  (w/postwalk
   (fn [form]
     (if (and (seq? form)
              (= (first form) 'qwerty/let*)
              (> (count (second form)) 1))
       (let [[_ [b & bs] body] form]
         `(qwerty/let* (~b)
                       (qwerty/let* ~bs
                                    ~body)))
       form))
   form))

(defn raise-lets [form]
  (w/postwalk
   (fn [form]
     (if (and (seq? form)
              (= (first form) 'qwerty/let*))
       (let [[_ [[n v]] body] form]
         (if (and (seq? v)
                  (= (first v) 'qwerty/let*))
           (let [[_ [[vn vv]] vbody] v]
             `(qwerty/let* ((~vn ~vv))
                           (qwerty/let* ((~n ~vbody))
                                        ~body)))
           form))
       form))
   form))

(defn f [form]
  (let [nf (raise-lets (collapse-do (split-lets (collapse-do (raise-decls form)))))]
    (if (= nf form)
      form
      (recur nf))))

(loop []
  (let [form (read)]
    (cond
     (and (seq? form) (= (first form) 'qwerty/package))
     (println "package " (second form))
     (and (seq? form) (= (first form) 'qwerty/import))
     (println "import " (pr-str (second form)))
     (and (seq? form) (= (first form) 'defgo))
     (printf "func %s (){\n}\n\n" (second form))
     :else (do
             #_(pprint (f (lower form)))
             (println)
             (go (f (lower form))))
     #_(let [{:keys [declarations expression]} (lower form)]
         (binding [*context* :statement
                   *scope* :package]
           (go declarations)
           (go expression)))))
  (println)
  (recur))
