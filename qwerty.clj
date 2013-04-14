#!/usr/bin/java -jar /Users/hiredman/src/clojure/target/clojure-1.5.0-master-SNAPSHOT.jar

(declare ^:dynamic *context*)
(declare ^:dynamic *scope*)

(defmulti lower type)

(defmulti lower-seq first)

(defmethod lower clojure.lang.Symbol [s]
  {:expression s})

(defmethod lower clojure.lang.ISeq [s]
  (lower-seq s))

(defmethod lower-seq 'qwerty/fn* [[_ args body]]
  (let [function-name (gensym 'fn)
        function-type-name (gensym 'Tfn)
        struct-name (gensym 'Sfn)
        local-name (gensym 'fn)
        lowered-body (lower body)]
    {:declarations `(qwerty/do
                      ~(:declarations lowered-body)
                      (qwerty/defgofun ~function-name ~args
                        ((~'string) ~'string)
                        ~(:expression lowered-body))
                      (qwerty/type ~function-type-name ((~'string)))
                      (qwerty/struct ~struct-name
                                     ~'_fun ~function-type-name))
     :expression `(qwerty/do
                    (qwerty/:= ~local-name (qwerty/new ~struct-name ~function-type-name))
                    ~local-name)}))

(defmethod lower-seq 'qwerty/defgofun [[_ function-name args types & body]]
  {:declarations `(qwerty/defgofun ~function-name ~args ~types
                    ~@(map (comp :expression lower) body))
   :expression nil})

(defmethod lower-seq 'qwerty/. [[_ func & args]]
  {:declarations nil
   :expression `(qwerty/. ~func ~@args)})

(defmethod lower-seq 'qwerty/do [[_ & body]]
  (-> (reduce
       (fn [m x]
         (let [{:keys [expression declarations]} (lower x)]
           (-> m
               (update-in [:declarations] conj declarations)
               (update-in [:expression] conj expression))))
       {:declarations ['qwerty/do]
        :expression ['qwerty/do]}
       body)
      (update-in [:declarations] seq)
      (update-in [:expression] seq)))

(defmethod lower-seq 'qwerty/let* [[_ bindings & body]]
  (if (seq bindings)
    (let [lowered-bindings (for [[n v] bindings]
                             [n (lower v)])
          lowered-body (lower (cons 'qwerty/do body))]
      {:declarations (concat (mapcat :declarations lowered-bindings)
                             (:declarations lowered-body))
       :expression (cons 'qwerty/do
                         (concat (for [[n {:keys [expression]}] lowered-bindings]
                                   (if (and (seq? expression)
                                            (= 'qwerty/do (first expression)))
                                     (let [x (butlast expression)]
                                       (concat x `(qwerty/:= ~n ~(last expression))))
                                     `(qwerty/:= ~n ~expression)))
                                 [(:expression lowered-body)]))})))

(defmethod lower-seq 'qwerty/.- [expr]
  {:expression expr})

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

(defmethod go-seq 'qwerty/:= [[_ n e]]
  (if (= *scope* :function)
    (do
      (print n ":=")
      (go e)
      (println))
    (do
      (print "var" n "=")
      (binding [*scope* :function]
        (go e))
      (println))))

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

(loop []
  (let [form (read)]
    (cond
     (and (seq? form) (= (first form) 'qwerty/package))
     (println "package " (second form))
     (and (seq? form) (= (first form) 'qwerty/import))
     (println "import " (pr-str (second form)))
     (and (seq? form) (= (first form) 'defgo))
     (printf "func %s (){\n}\n\n" (second form))
     :else (let [{:keys [declarations expression]} (lower form)]
             (binding [*context* :statement
                       *scope* :package]
               (go declarations)
               (go expression)))))
  (println)
  (recur))
