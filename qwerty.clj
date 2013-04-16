#!/usr/bin/java -jar /Users/hiredman/src/clojure/target/clojure-1.5.0-master-SNAPSHOT.jar

(require '[clojure.walk :as w]
         '[clojure.pprint :refer [pprint]]
         '[clojure.set :as s])

(declare ^:dynamic *context*)
(declare ^:dynamic *scope*)

(defmulti α-convert (fn [exp env] (type exp)))
(defmulti α-convert-seq (fn [exp env] (first exp)))

(defmethod α-convert clojure.lang.Symbol [s env]
  (if (contains? env s)
    (get env s)
    s))

(defmethod α-convert :default [s env] s)

(defmethod α-convert clojure.lang.ISeq [s env]
  (α-convert-seq s env))

(defmethod α-convert-seq 'qwerty/struct [s env]
  s)

(defmethod α-convert-seq 'qwerty/new [s env]
  s)

(defmethod α-convert-seq 'qwerty/local [s env]
  s)

(defmethod α-convert-seq 'qwerty/cast [[_ t v] env]
  `(qwerty/cast ~t ~(α-convert v env)))

(defmethod α-convert-seq 'qwerty/nil? [[_ v] env]
  `(qwerty/nil? ~(α-convert v env)))

(defmethod α-convert-seq 'qwerty/goderef [[_ v] env]
  `(qwerty/goderef ~(α-convert v env)))

(defmethod α-convert-seq 'qwerty/+ [[_ a b] env]
  `(qwerty/+ ~(α-convert a env) ~(α-convert b env)))

(defmethod α-convert-seq 'qwerty/set! [[_ f v] env]
  `(qwerty/set! ~(α-convert f env) ~(α-convert v env)))

(defmethod α-convert-seq 'qwerty/.- [[_ target field] env]
  `(qwerty/.- ~(α-convert target env) ~field))

(defmethod α-convert-seq 'qwerty/. [[_ function & args] env]
  `(qwerty/. ~function ~@(map #(α-convert % env) args)))

(defmethod α-convert-seq 'qwerty/godef [[_ n body] env]
  `(qwerty/godef ~n ~(α-convert body env)))

(defmethod α-convert-seq 'qwerty/fn* [[_ args body] env]
  (let [args (for [a args]
               [a (gensym a)])]
    `(qwerty/fn* ~(map second args)
                 ~(α-convert body (into env args)))))

(defmethod α-convert-seq 'qwerty/results [[_ values exp body] env]
  (let [args (for [a values]
               [a (gensym a)])]
    `(qwerty/results ~(map second args)
                     ~(α-convert exp env)
                     ~(α-convert body (into env args)))))

(defmethod α-convert-seq 'qwerty/defgofun [[_ name args types body] env]
  (let [args (for [a args]
               [a (gensym a)])]
    `(qwerty/defgofun
       ~name
       ~(map second args)
       ~types
       ~(α-convert body (into env args)))))

(defmethod α-convert-seq 'qwerty/do [[_ & body] env]
  `(qwerty/do ~@(map #(α-convert % env) body)))

(defmethod α-convert-seq 'qwerty/values [[_ & body] env]
  `(qwerty/values ~@(map #(α-convert % env) body)))

(defmethod α-convert-seq 'qwerty/let* [[_ bindings body] env]
  (let [{:keys [bindings env]} (reduce
                                (fn [{:keys [bindings env]} [n v]]
                                  (let [nn (gensym n)]
                                    {:bindings (conj bindings (list nn (α-convert v env)))
                                     :env (assoc env n nn)}))
                                {:bindings []
                                 :env env} bindings)]
    `(qwerty/let* ~(seq bindings)
                  ~(α-convert body env))))


(defmethod α-convert-seq :default [exp env]
  (assert (not (and (symbol? (first exp))
                    (= "qwerty" (namespace (first exp)))))
          (first exp))
  (map #(α-convert % env) exp))

(defmulti free-variables type)
(defmulti free-variables-seq first)

(defmethod free-variables clojure.lang.Symbol [s]
  #{s})

(defmethod free-variables java.lang.Number [s]
  #{})

(defmethod free-variables java.lang.String [s]
  #{})

(defmethod free-variables clojure.lang.ISeq [exp]
  (free-variables-seq exp))

(defmethod free-variables nil [exp]
  #{})

(defmethod free-variables-seq 'qwerty/. [[_ func & args]]
  (set (mapcat free-variables args)))

(defmethod free-variables-seq 'qwerty/local [_]
  #{})

(defmethod free-variables-seq 'qwerty/goderef [[_ v]]
  #{})

(defmethod free-variables-seq 'qwerty/new [_]
  #{})

(defmethod free-variables-seq 'qwerty/comment [_]
  #{})

(defmethod free-variables-seq 'qwerty/do [[_ & body]]
  (set (mapcat free-variables body)))

(defmethod free-variables-seq 'qwerty/set! [[_ f v]]
  (free-variables v))

(defmethod free-variables-seq 'qwerty/.- [_]
  #{})

(defmethod free-variables-seq 'qwerty/cast [[_ c v]]
  (free-variables v))

(defmethod free-variables-seq 'qwerty/results [[_ locals exp body]]
  (into (free-variables exp)
        (s/difference (free-variables body) (set locals))))

(defmethod free-variables-seq 'qwerty/values [[_ & args]]
  (set (mapcat free-variables args)))

(defmethod free-variables-seq 'qwerty/let* [[_ bindings body]]
  (:free (let [b (gensym)]
           (reduce
            (fn [{:keys [free bound]} [n value]]
              (let [free (disj (into free (s/difference (free-variables value) bound)) b)
                    bound (conj bound n)]
                {:free free
                 :bound bound}))
            {:free #{}
             :bound #{}} (concat bindings [[b body]])))))

(defmethod free-variables-seq 'qwerty/+ [[_ a b]]
  (into (free-variables a)
        (free-variables b)))

(defmulti close-over (fn [exp variables this-name] (type exp)))
(defmulti close-over-seq (fn [exp variables this-name] (first exp)))

(defmethod close-over clojure.lang.Symbol [exp variables this-name]
  (if (contains? variables exp)
    `(qwerty/let* ((t# (qwerty/.- ~this-name ~exp)))
                  t#)
    exp))

(defmethod close-over String [exp variables this-name]
  exp)

(defmethod close-over Long [exp variables this-name]
  exp)

(defmethod close-over nil [& _] nil)

(defmethod close-over clojure.lang.ISeq [exp variables this-name]
  (close-over-seq exp variables this-name))

(defmethod close-over-seq 'qwerty/. [[_ func & args] variables this-name]
  `(qwerty/. ~func ~@(for [a args] (close-over a variables this-name))))

(defmethod close-over-seq 'qwerty/.- [exp variables this-name]
  exp)

(defmethod close-over-seq 'qwerty/comment [exp variables this-name]
  exp)

(defmethod close-over-seq 'qwerty/do [[_ & body] variables this-name]
  `(qwerty/do ~@(for [expr body] (close-over expr variables this-name))))

(defmethod close-over-seq 'qwerty/+ [[_ a b] variables this-name]
  `(qwerty/+ ~(close-over a variables this-name)
             ~(close-over b variables this-name)))

(defmethod close-over-seq 'qwerty/let* [[_ bindings body] variables this-name]
  (let [b (gensym)
        r (reduce
           (fn [b [n value]]
             (let [bound (set (map first b))]
               (conj b [n (close-over value (s/difference variables bound) this-name)])))
           []
           (concat bindings [[b body]]))]
    `(qwerty/let* ~(butlast r)
                  ~(second (last r)))))

(defmethod close-over-seq 'qwerty/new [expr variables this-name]
  expr)

(defmethod close-over-seq 'qwerty/local [expr variables this-name]
  expr)

(defmethod close-over-seq 'qwerty/goderef [expr variables this-name]
  expr)


(defmethod close-over-seq 'qwerty/values [[_ & args] variables this-name]
  `(qwerty/values ~@(for [a args] (close-over a variables this-name))))

(defmethod close-over-seq 'qwerty/set! [[_ f v] variables this-name]
  `(qwerty/set! ~f ~(close-over v variables this-name)))

(defmethod close-over-seq 'qwerty/cast [[_ t v] variables this-name]
  `(qwerty/cast ~t ~(close-over v variables this-name)))

(defmulti return-count type)
(defmulti return-count-seq first)

(defmethod return-count clojure.lang.ISeq [s]
  (return-count-seq s))

(defmethod return-count :default [s]
  [1])

(defmethod return-count-seq 'qwerty/values [[_ & args]]
  [(count args)])

(defmethod return-count-seq 'qwerty/fn* [[_ args body]]
  (return-count body))

(defmethod return-count-seq 'qwerty/let* [[_ bindings body]]
  (return-count body))

(defmethod return-count-seq 'qwerty/do [[_ & body]]
  (mapcat return-count body))

(defmethod return-count-seq 'qwerty/set! [_]
  [1])

(defmethod return-count-seq 'qwerty/. [_]
  [1])

(defmethod return-count-seq 'qwerty/.- [_]
  [1])

(defmethod return-count-seq 'qwerty/+ [_]
  [1])

(defmulti lower type)

(defmulti lower-seq first)

(defmethod lower clojure.lang.Symbol [s]
  s)

(defmethod lower java.lang.String [s]
  s)

(defmethod lower java.lang.Number [s]
  s)

(defmethod lower nil [s] nil)

(defmethod lower clojure.lang.ISeq [s]
  (lower-seq s))

(defmethod lower-seq 'qwerty/fn* [form]
  (let [[_ args body] form
        function-name (gensym 'fn)
        function-type-name (gensym 'Tfn)
        struct-name (gensym 'Sfn)
        struct-pointer (symbol (str "*" (name struct-name)))
        local-name (gensym 'fn)
        constructor (gensym 'Cfn)
        lowered-body (lower body)
        free-in-body (remove (set args) (free-variables lowered-body))
        this-name (gensym 'this)
        return-types (repeat (apply max (return-count form)) 'interface)]
    `(qwerty/do
       (qwerty/defgofun ~function-name ~(cons this-name args)
         ((~struct-pointer ~@(repeat (count args) 'interface)) ~return-types)
         (qwerty/do
           (qwerty/comment "line" ~(:line (meta form)))
           ~(close-over lowered-body (set free-in-body) this-name)))
       (qwerty/type ~function-type-name ((~struct-pointer ~@(repeat (count args) 'interface)) ~return-types))
       (qwerty/struct ~struct-name
                      ~@(for [v free-in-body
                              i [v 'interface]]
                          i)
                      ~'_fun ~function-type-name)
       (qwerty/defgofun ~constructor ~(seq free-in-body)
         (~(repeat (count free-in-body) 'interface) ~(symbol (str "*" (name struct-name))))
         (qwerty/do
           (qwerty/let* ((~'c (qwerty/new ~struct-name)))
                        (qwerty/do
                          ~@(for [v free-in-body]
                              `(qwerty/set! (qwerty/.- ~'c ~v) ~v))
                          (qwerty/set! (qwerty/.- ~'c ~'_fun) ~function-name)
                          ~'c))))
       (qwerty/let* ((~local-name (qwerty/. ~constructor ~@free-in-body)))
                    (qwerty/do
                      ~local-name)))))

(defmethod lower-seq 'qwerty/defgofun [[_ function-name args types & body]]
  `(qwerty/defgofun ~function-name ~args ~types
     (qwerty/do ~@(map lower body))))

(defmethod lower-seq 'qwerty/. [[_ func & args]]
  `(qwerty/do
     (qwerty/comment ".")
     (qwerty/. ~func ~@args)))

(defmethod lower-seq 'qwerty/do [[_ & body]]
  (cons 'qwerty/do (map lower body)))

(defmethod lower-seq 'qwerty/let* [[_ bindings & body]]
  `(qwerty/let* ~(for [[n v] bindings]
                   `(~n ~(lower v)))
                (qwerty/do
                  ~@(map lower body))))

(defmethod lower-seq 'qwerty/.- [expr]
  expr)

(defmethod lower-seq 'qwerty/struct [expr]
  expr)

(defmethod lower-seq 'qwerty/new [expr]
  expr)

(defmethod lower-seq 'qwerty/cast [[_ type v]]
  (let [c (gensym 'c)]
    `(qwerty/let* ((~c ~(lower v)))
                  (qwerty/cast ~type ~c))))

(defmethod lower-seq 'qwerty/local [exp]
  exp)

(defmethod lower-seq 'qwerty/comment [exp]
  exp)

(defmethod lower-seq 'qwerty/values [[_ & args]]
  (let [args (for [a args]
               (list (gensym 'a) (lower a)))]
    `(qwerty/let* (~@args)
                  (qwerty/values ~@(map first args)))))

(defmethod lower-seq 'qwerty/nil? [[_ e]]
  (let [r (gensym 'r)]
    `(qwerty/let* ((~r ~(lower e)))
                  (qwerty/nil? ~r))))

(defmethod lower-seq 'qwerty/set! [[_ f v]]
  (let [r (gensym 'v)]
    `(qwerty/let* ((~r ~(lower v)))
                  (qwerty/set! ~f ~r))))

(defmethod lower-seq 'qwerty/+ [[_ a b]]
  (let [a_ (gensym 'a)
        b_ (gensym 'b)]
    `(qwerty/let* ((~a_ ~(lower a))
                   (~b_ ~(lower b)))
                  (qwerty/+ ~a_ ~b_))))

(defmethod lower-seq 'qwerty/godef [[_ n v]]
  (let [a_ (gensym 'v)]
    `(qwerty/let* ((~a_ ~(lower v)))
                  (qwerty/godef ~n ~a_))))

(defmethod lower-seq 'qwerty/results [[_ values [op & args] body]]
  (if (= op 'qwerty/.)
    (let [m (first args)
          args (for [a (rest args)]
                 (list (gensym 'a) (lower a)))]
      `(qwerty/let* ~args
                    (qwerty/results ~values (qwerty/. ~m ~@(map first args))
                                    ~(lower body))))
    (let [op-n (gensym 'op)
          args (for [a args]
                 (list (gensym 'a) (lower a)))
          v (gensym 'v)
          f (gensym 'f)]
      `(qwerty/let* ((~op-n ~(lower op))
                     ~@args
                     (~f (qwerty/.- ~op-n ~'_fun)))
                    (qwerty/results ~values (qwerty/. ~f ~op-n ~@(map first args))
                                    ~(lower body))))))

(defmethod lower-seq :default [form]
  (let [[fun & args] form]
    (assert (not= "qwerty" (namespace fun)) fun)
    (let [f (gensym 'f)
          lowered-args (map lower args)
          bound-args (for [a args]
                       (if (symbol? a)
                         (list a a)
                         (list (gensym 'arg) a)))]
      (lower `(qwerty/let* ((~f (qwerty/.- ~fun ~'_fun))
                            ~@(remove (fn [[n v]] (= n v)) bound-args))
                           (qwerty/do
                             (qwerty/comment "line" ~(:line (meta form)))
                             (qwerty/. ~f ~fun ~@(map first bound-args))))))))

(defmulti go type)

(defmulti go-seq first)

(defmethod go clojure.lang.ISeq [s]
  (go-seq s))

(defmethod go java.lang.String [s]
  (print " ")
  (pr s)
  (print " "))

(defmethod go java.lang.Long[s]
  (print " ")
  (pr s)
  (print " "))

(defmethod go clojure.lang.Symbol [s]
  (when (not= *scope* :package)
    (if (= :return *context*)
      (print "return"))
    (print (str " " (munge s) " "))
    (if (= :return *context*)
      (println))))

(defmethod go nil [s]
  (when (not= *scope* :package)
    (if (= :return *context*)
      (print "return"))
    (print " nil ")
    (if (= :return *context*)
      (println))))

(defmethod go-seq 'qwerty/do [s]
  (doseq [item (butlast (rest s))]
    (binding [*context* :statement]
      (go item)
      (println)))
  (go (last s)))

(defmethod go-seq 'qwerty/defgofun [[_ fun-name args types & body]]
  (binding [*scope* :function]
    (println "func" fun-name (str "(" (apply str (interpose \, (map
                                                                (fn [arg-name type]
                                                                  (str arg-name " "
                                                                       (if (= type 'interface)
                                                                         "interface {}"
                                                                         type)))
                                                                args (first types))))
                                  ")") (str "(" (when (> (count types) 1)
                                                  (if (seq? (last types))
                                                    (apply str
                                                           (interpose \,
                                                                      (for [i (last types)]
                                                                        (if (= 'interface i)
                                                                          "interface {}"
                                                                          i))))
                                                    (if (= 'interface (last types))
                                                      "interface {}"
                                                      (last types))))
                                            ")") "{")
    (binding [*context* (if (> (count types) 1)
                          :return
                          :statement)]
      (go (cons 'qwerty/do body)))
    (when-not (> (count types) 1)
      (println))
    (println "}")
    (println)))

(defmethod go-seq 'qwerty/type [[_ type-name type-expression]]
  (assert (seq? type-expression))
  (print "type" type-name "func")
  (print "(" (apply str (interpose \, (for [x (first type-expression)]
                                        (if (= x 'interface)
                                          "interface {}"
                                          x)))) ") ")
  (print (str "(" (if (seq? (last type-expression))
                    (apply str
                           (interpose \,
                                      (for [i (last type-expression)]
                                        (if (= 'interface i)
                                          "interface {}"
                                          i))))
                    (if (= 'interface (last type-expression))
                      "interface {}"
                      (last type-expression)))
              ")"))
  (println)
  (println))


(defmethod go-seq 'qwerty/struct [[_ struct-name & fields]]
  (println "type" struct-name "struct {")
  (doseq [[field-name field-type] (partition-all 2 fields)]
    (println " " field-name (if (= 'interface field-type)
                              "interface {}"
                              field-type)))
  (println "}")
  (println))

(defmethod go-seq 'qwerty/new [[_ struct-name & values]]
  (if (= :return *context*)
    (print "return"))
  (print " new(" struct-name)
  (doseq [v values]
    (go v))
  (print ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/let*  [[_ [[n v]] body]]
  (cond
   (and (seq? v)
        (= 'qwerty/let* (first v)))
   (do
     (binding [*context* :statement]
       (go v))
     (println)
     (recur `(qwerty/let* ((~n ~(last v))) ~body)))
   (nil? v)
   (do
     (println "var" n "interface{}" "=" "nil")
     (go body))
   (= *scope* :function)
   (do
     (print " " n ":=")
     (binding [*context* :statement]
       (go v))
     (println)
     (go body))
   :else
   (do
     (print "var " (munge n) "=")
     (binding [*context* :statement]
       (go v))
     (println)
     (go body))))

(defmethod go-seq 'qwerty/. [[_ func & args]]
  (if (= :return *context*)
    (print "return"))
  (print "" func "(")
  (binding [*context* :statement]
    (doseq [v args]
      (go v)
      (print ",")))
  (print ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/.- [[_ obj field]]
  (if (= :return *context*)
    (print "return"))
  (print (str " " (munge obj) "." (munge field) " "))
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/set! [[_ thing value]]
  (binding [*context* :statement]
    (go thing))
  (print " = ")
  (binding [*context* :statement]
    (go value))
  (println))

(defmethod go-seq 'qwerty/goderef [[_ thing]]
  (if (= :return *context*)
    (print "return"))
  (print " &")
  (binding [*context* :statement]
    (go thing))
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/cast [[_ t n]]
  (if (= :return *context*)
    (print "return"))
  (print (str n ".("t ")"))
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/+ [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (print "(" a "+" b ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/values [[_  & args]]
  (assert (= *context* :return))
  (println "return" (apply str (interpose \, args))))

(defmethod go-seq 'qwerty/results [[_  names exp body]]
  (print (apply str (interpose \, names)) ":=")
  (go exp)
  (println)
  (go body))

(defmethod go-seq 'qwerty/godef [[_ n b]]
  (println "var" (munge n) "=" b))

(defmethod go-seq 'qwerty/comment [[_ & args]]
  (println "/*" (apply print-str args) "*/"))

(defmethod go-seq 'qwerty/local [[_ n type]]
  (println "var" n type))

(defmethod go-seq 'qwerty/nil? [[_ v]]
  (println "(" v "== nil" ")"))

(defmulti raise-decls type)

(defmethod raise-decls :default [s] s)

(defmulti raise-decls-seq first)

(defmethod raise-decls clojure.lang.ISeq [s]
  (raise-decls-seq s))

(defn decl? [form]
  (and (seq? form)
       ('#{qwerty/defgofun
           qwerty/type
           qwerty/struct
           } (first form))))

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
              (= (first form) 'qwerty/let*)
              (= 1 (count (second form))))
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

(let [eof (Object.)]
  (loop [form (read *in* false eof)]
    (when-not (= eof form)
      (cond
       (and (seq? form) (= (first form) 'qwerty/package))
       (println "package " (second form))
       (and (seq? form) (= (first form) 'qwerty/import))
       (println "import " (pr-str (second form)))
       :else (do
               ;; (pprint (f (lower form)))
               ;; (println)
               (go (f (lower (α-convert form {})))))
       #_(let [{:keys [declarations expression]} (lower form)]
           (binding [*context* :statement
                     *scope* :package]
             (go declarations)
             (go expression))))
      (println)
      (recur (read *in* false eof)))))
