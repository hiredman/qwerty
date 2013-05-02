#!/usr/bin/java -jar /Users/hiredman/src/clojure/target/clojure-1.5.0-master-SNAPSHOT.jar

(require '[clojure.walk :as w]
         '[clojure.pprint :refer [pprint *print-right-margin*]]
         '[clojure.set :as s]
         '[clojure.java.io :as io])

(ns-unmap *ns* 'gensym)

(defn gensym [s]
  (clojure.core/gensym (symbol (str s (mod (System/currentTimeMillis) 10000)))))

(declare ^:dynamic *context*)
(declare ^:dynamic *scope*)
(declare ^:dynamic *package*)

(def global-env (atom #{}))

(load-file "./expand.clj")
(load-file "./alpha.clj")
(load-file "./free.clj")
(load-file "./var.clj")

(defn fn-interface []
  (if (= *package* 'qwerty)
    "IFn"
    "qwerty.IFn"))

;; close-over

(defmulti close-over (fn [exp variables this-name] (type exp)))
(defmulti close-over-seq (fn [exp variables this-name] (first exp)))

(defmethod close-over clojure.lang.Symbol [exp variables this-name]
  (if (contains? variables exp)
    (doall `(qwerty/.- ~this-name ~exp))
    exp))

(defmethod close-over String [exp variables this-name]
  exp)

(defmethod close-over Long [exp variables this-name]
  exp)

(defmethod close-over Boolean [exp variables this-name]
  exp)

(defmethod close-over Character [exp variables this-name]
  exp)

(defmethod close-over nil [& _] nil)

(defmethod close-over clojure.lang.ISeq [exp variables this-name]
  (close-over-seq exp variables this-name))

(defmethod close-over-seq 'qwerty/. [[_ func & args] variables this-name]
  (doall `(qwerty/. ~func ~@(doall (for [a args] (close-over a variables this-name))))))

(defmethod close-over-seq 'qwerty/.- [exp variables this-name]
  exp)

(defmethod close-over-seq 'qwerty/comment [exp variables this-name]
  exp)

(defmethod close-over-seq 'qwerty/do [[_ & body] variables this-name]
  `(qwerty/do ~@(doall (for [expr body] (close-over expr variables this-name)))))

(defmethod close-over-seq 'qwerty/+ [[_ a b] variables this-name]
  `(qwerty/+ ~(close-over a variables this-name)
             ~(close-over b variables this-name)))

(defmethod close-over-seq 'qwerty/nth* [[_ a b] variables this-name]
  `(qwerty/nth* ~(close-over a variables this-name)
                ~(close-over b variables this-name)))

(defmethod close-over-seq 'qwerty/- [[_ a b] variables this-name]
  `(qwerty/- ~(close-over a variables this-name)
             ~(close-over b variables this-name)))

(defmethod close-over-seq 'qwerty/* [[_ a b] variables this-name]
  `(qwerty/* ~(close-over a variables this-name)
             ~(close-over b variables this-name)))

(defmethod close-over-seq 'qwerty/let* [[_ bindings body :as form] variables this-name]
  (let [b (gensym)
        r (reduce
           (fn [b [n value]]
             (let [bound (set (map first b))]
               (conj b [n (close-over value (s/difference variables bound) this-name)])))
           []
           (concat bindings [[b body]]))]
    (with-meta
      `(qwerty/let* ~(butlast r)
                    ~(second (last r)))
      (meta form))))

(defmethod close-over-seq 'qwerty/new [expr variables this-name]
  expr)

(defmethod close-over-seq 'qwerty/local [expr variables this-name]
  expr)


(defmethod close-over-seq 'qwerty/goderef [expr variables this-name]
  expr)

(defmethod close-over-seq 'qwerty/go-method-call [[_ target method-name & args] variables this-name]
  `(qwerty/go-method-call ~(close-over target variables this-name)
                          ~method-name
                          ~@(for [arg args]
                              (close-over arg variables this-name))))


(defmethod close-over-seq 'qwerty/values [[_ & args] variables this-name]
  `(qwerty/values ~@(doall (for [a args] (close-over a variables this-name)))))

(defmethod close-over-seq 'qwerty/set! [[_ f v] variables this-name]
  `(qwerty/set! ~f ~(close-over v variables this-name)))

(defmethod close-over-seq 'qwerty/cast [[_ t v] variables this-name]
  `(qwerty/cast ~t ~(close-over v variables this-name)))

(defmethod close-over-seq 'qwerty/map-entry [[_ m k] variables this-name]
  `(qwerty/map-entry ~(close-over m variables this-name)
                     ~(close-over k variables this-name)))

(defmethod close-over-seq 'qwerty/results [[_ values app body] variables this-name]
  `(qwerty/results ~values
                   ~(close-over app variables this-name)
                   ~(close-over body variables this-name)))

(defmethod close-over-seq 'qwerty/if [[_ cond then else] variables this-name]
  `(qwerty/if ~(close-over cond variables this-name)
     ~(close-over then variables this-name)
     ~(close-over else variables this-name)))

(defmethod close-over-seq 'qwerty/test [[_ condition exp] variables this-name]
  `(qwerty/test ~(close-over condition variables this-name)
                ~exp))

(defmethod close-over-seq 'qwerty/labels [[_ & exps] variables this-name]
  `(qwerty/labels ~@(for [e exps]
                      (if (symbol? e)
                        e
                        (close-over e variables this-name)))))

(defmethod close-over-seq 'qwerty/struct [expr variables this-name]
  expr)

(defmethod close-over-seq 'qwerty/goto [form variables this-name]
  form)

(defmethod close-over-seq 'qwerty/nil? [[_ v] variables this-name]
  `(qwerty/nil? ~(close-over v variables this-name)))

(defmethod close-over-seq 'qwerty/= [[_ a b] variables this-name]
  `(qwerty/= ~(close-over a variables this-name)
             ~(close-over b variables this-name)))

(defmethod close-over-seq 'qwerty/go<- [[_ [result channel] body] variables this-name]
  `(qwerty/go<- (~result ~(close-over channel variables this-name))
                ~(close-over body variables this-name)))

(defmethod close-over-seq 'qwerty/goref [exp variables this-name]
  exp)

(defmethod close-over-seq 'qwerty/map-update [[_ map key value] variables this-name]
  `(qwerty/map-update ~(close-over map variables this-name)
                      ~(close-over key variables this-name)
                      ~(close-over value variables this-name)))

(defmethod close-over-seq 'qwerty/func [exp variables this-name]
  exp)

(defmethod close-over-seq :default [form variables this-name]
  (assert (not (and (symbol? (first form))
                    (= "qwerty" (namespace (first form)))))
          (pr-str form))
  (doall
   (for [p form]
     (close-over p variables this-name))))

;; return-count

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

(defmethod return-count-seq 'qwerty/map-entry [_]
  [1])

(defmethod return-count-seq 'qwerty/results [[_ exp app body]]
  (return-count body))

(defmethod return-count-seq 'qwerty/go<- [[_ _ body]]
  (return-count body))

(defmethod return-count-seq 'qwerty/labels [[_ & exprs]]
  [(apply max
          (for [e exprs
                n (if (symbol? e)
                    [1]
                    (return-count e))]
            n))])

(defmethod return-count-seq 'qwerty/test [_]
  [1])

(defmethod return-count-seq 'qwerty/goto [_]
  [1])

(defmethod return-count-seq 'qwerty/cast [_]
  [1])

(defmethod return-count-seq 'qwerty/go-method-call [_]
  [1])

(defmethod return-count-seq 'qwerty/map-update [_]
  [1])

(defmethod return-count-seq 'qwerty/- [_]
  [1])

(defmethod return-count-seq 'qwerty/* [_]
  [1])

(defmethod return-count-seq 'qwerty/nth* [_]
  [1])

(defmethod return-count-seq 'qwerty/goref [_]
  [1])

(defmethod return-count-seq 'qwerty/quote [_]
  [1])

(defmethod return-count-seq 'qwerty/go-> [[_ [ch value] body]]
  (return-count body))

(defmethod return-count-seq :default [x]
  (assert (and (seq? x)
               (not (and (symbol? (first x))
                         (= "qwerty" (namespace (first x))))))
          (pr-str x))
  (mapcat return-count x))

(defmethod return-count-seq 'qwerty/if [[_ cond then else]]
  (concat (return-count then)
          (return-count else)))

;; lower
;; lowers lisp features in to go features
;; lisp functions are structs (environment) and a go function pointer
;; function invocation is invoking function pointer

(defmulti lower type)

(defmulti lower-seq first)

(defmethod lower clojure.lang.Symbol [s]
  s)

(defmethod lower java.lang.String [s]
  s)

(defmethod lower java.lang.Boolean [s]
  s)

(defmethod lower java.lang.Number [s]
  s)

(defmethod lower java.lang.Character [s]
  s)

(defmethod lower clojure.lang.PersistentVector [v]
  (lower (seq v)))

(defmethod lower nil [s] nil)

(defmethod lower clojure.lang.ISeq [s]
  (if (empty? s)
    nil
    (lower-seq s)))

(def max-arity 6)
(def max-returns 4)

(defmethod lower-seq 'qwerty/fn* [form]
  (let [[_ args body] form
        function-type-name (gensym 'Tfn)
        struct-name (gensym 'Sfn)
        struct-pointer (symbol (str "*" (name struct-name)))
        local-name (gensym 'fn)
        constructor (gensym 'Cfn)
        lowered-body (lower body)
        free-in-body (distinct (remove (set args) (free-variables body)))
        this-name (gensym 'this)
        return-types (repeat (apply max (return-count form)) 'interface)]
    `(qwerty/do
       (qwerty/comment "free-in-body" ~(pr-str free-in-body))
       (qwerty/struct ~struct-name
                      ~@(doall (for [v free-in-body]
                                 `(qwerty/T ~v ~'interface))))
       ~@(for [arg-count (range 0 (inc max-arity))
               return-count (range 1 (inc max-returns))
               :let [function-name (symbol (str "Invoke" arg-count "_" return-count))
                     impl-function-name (symbol (str "Invoke" (count args) "_" (count return-types)))]]
           (cond
            (and (= arg-count (count args))
                 (= return-count (count return-types)))
            `(qwerty/func (qwerty/T ~this-name (~'* ~struct-name)) ~function-name
                          ~(for [a args]
                             `(qwerty/T ~a ~'interface))
                          ~(repeat return-count `(qwerty/T _# ~'interface))
                          (~(if (= 1 return-count)
                              `qwerty/return
                              `qwerty/do)
                           (qwerty/do
                             (qwerty/comment "line" ~(:line (meta form)))
                             ~(close-over lowered-body (set free-in-body) this-name))))
            (and (= arg-count (count args))
                 (< return-count (count return-types)))
            (let [values (repeatedly (count return-types) #(gensym 'values))]
              `(qwerty/func (qwerty/T ~this-name (~'* ~struct-name)) ~function-name
                            ~(for [a args]
                               `(qwerty/T ~a ~'interface))
                            ~(repeat return-count `(qwerty/T _# ~'interface))
                            (qwerty/results ~values (qwerty/go-method-call ~this-name ~impl-function-name ~@args)
                                            (qwerty/do
                                              ~@(for [v (drop return-count values)]
                                                  `(qwerty/. ~'NOP ~v))
                                              (qwerty/values ~@(take return-count values))))))
            :else
            `(qwerty/func (qwerty/T ~this-name (~'* ~struct-name)) ~function-name
                          ~(for [a (repeatedly arg-count #(gensym 'arg))]
                             `(qwerty/T ~a ~'interface))
                          ~(repeat return-count `(qwerty/T _# ~'interface))
                          (qwerty/do
                            (qwerty/. ~'panic ~(str "bad arity " arg-count "-" return-count))
                            (qwerty/values ~@(repeat return-count nil))))))
       (qwerty/func (qwerty/T ~'s ~struct-name) ~'String () ((qwerty/T _# ~'string))
                    (qwerty/return ~(str "#<IFn " struct-name ">")))
       (qwerty/func ~constructor ~(for [t free-in-body]
                                    `(qwerty/T ~t ~'interface))
                    ((qwerty/T _# (~'* ~struct-name)))
                    ~(lower
                      `(qwerty/let* ((~'c (qwerty/new ~struct-name)))
                                    (qwerty/do
                                      ~@(doall (for [v free-in-body]
                                                 `(qwerty/set! (qwerty/.- ~'c ~v) ~v)))
                                      (qwerty/return ~'c)))))
       (qwerty/. ~constructor ~@(doall free-in-body)))))

(defmethod lower-seq 'qwerty/func [[_ name-or-target-type :as exp]]
  (if (symbol? name-or-target-type)
    (let [[_ name args returns body] exp]
      `(qwerty/func ~name ~args ~returns ~(lower body)))
    (let [[_ target name args returns body] exp]
      `(qwerty/func ~target ~name ~args ~returns ~(lower body)))))

;; TODO: multi value return
(defmethod lower-seq 'qwerty/return [[_ v]]
  (if (coll? v)
    (let [x (gensym 'foo)]
      (lower
       `(qwerty/let* ((~x ~(lower v)))
                     (qwerty/return ~x))))
    `(qwerty/return ~v)))

(defmethod lower-seq 'qwerty/. [[_ func & args]]
  (if (every? (complement coll?) args)
    `(qwerty/. ~func ~@(doall args))
    (let [bindings (for [a args]
                     (if (symbol? a)
                       (list a a)
                       (list (gensym 'a) (lower a))))]
      (lower
       `(qwerty/let* ~(remove #(= (first %) (second %)) bindings)
                     (qwerty/. ~func ~@(map first bindings)))))))

(defmethod lower-seq 'qwerty/do [[_ & body]]
  (cons 'qwerty/do (doall (map lower body))))

(defmethod lower-seq 'qwerty/let* [[_ bindings & body :as form]]
  (assert (not-any? (partial = 'qwerty/set!) (first bindings))
          (pr-str `(qwerty/let* ~bindings ~@body)))
  `(qwerty/do
     (qwerty/comment "line" ~(:line (meta form)))
     ~@(doall (for [[n v] bindings]
                (cond
                 (and (seq? v)
                      (= 'qwerty/cast (first v)))
                 `(qwerty/local ~n ~(second v))
                 (and (seq? v)
                      (= 'qwerty/new (first v)))
                 `(qwerty/local ~n ~(symbol (str "*" (name (second v)))))
                 :else
                 `(qwerty/local ~n ~'interface))))
     ~@(doall
        (for [[n v] bindings]
          (lower `(qwerty/set! ~n ~v))))
     ~@(doall
        (map lower body))))

(defmethod lower-seq 'qwerty/.- [[_ v f]]
  (if (and (coll? v)
           (not (= 'qwerty/goref (first v))))
    (let [o (gensym 'o)]
      `(qwerty/let* ((~o ~(lower v)))
                    (qwerty/.- ~o ~f)))
    `(qwerty/.- ~v ~f)))

(defmethod lower-seq 'qwerty/struct [expr]
  expr)

(defmethod lower-seq 'qwerty/new [expr]
  expr)

(defmethod lower-seq 'qwerty/goderef [[_ v]]
  (if (coll? v)
    (let [n (gensym 'deref)]
      (lower `(qwerty/let* ((~n ~(lower v)))
                           (qwerty/goderef ~n))))
    `(qwerty/goderef ~v)))

(defn atomic? [exp]
  (or (not (coll? exp))
      (and (every? (complement coll?) exp)
           (symbol? (first exp))
           (= "qwerty" (namespace (first exp)))
           (not= 'qwerty/do (first exp))
           (not= 'qwerty/results (first exp))
           (not= 'qwerty/values (first exp))
           (not= 'qwerty/commment (first exp)))))

(defmethod lower-seq 'qwerty/cast [[_ type v]]
  (if-not (coll? v)
    `(qwerty/cast ~type ~v)
    (let [c (gensym 'c)]
      (lower `(qwerty/let* ((~c ~v))
                           (qwerty/cast ~type ~c))))))

(defmethod lower-seq 'qwerty/local [exp]
  exp)

(defmethod lower-seq 'qwerty/comment [exp]
  exp)

(defmethod lower-seq 'qwerty/values [[_ & args]]
  (if (every? (complement coll?) args)
    `(qwerty/values ~@args)
    (let [args (for [a args]
                 (list (gensym 'a) (lower a)))]
      (lower `(qwerty/let* (~@(doall args))
                           (qwerty/values ~@(doall (map first args))))))))

(defmethod lower-seq 'qwerty/nil? [[_ e]]
  (if (or (coll? e)
          (nil? e))
    (let [r (gensym 'r)]
      `(qwerty/let* ((~r ~e))
                    (qwerty/nil? ~r)))
    `(qwerty/nil? ~e)))

(defmethod lower-seq 'qwerty/set! [[_ f v]]
  (let [lv (lower v)]
    (cond
     (atomic? lv)
     `(qwerty/set! ~f ~lv)
     (= 'qwerty/do (first lv))
     `(qwerty/do ~@(rest (butlast lv))
                 ~(lower `(qwerty/set! ~f ~(lower (last lv)))))
     (= 'qwerty/set! (first lv))
     (lower
      `(qwerty/do
         ~(lower lv)
         (qwerty/set! ~f nil)))
     (= 'qwerty/set! (first lv))
     (lower
      `(qwerty/set! ~f (qwerty/do ~lv nil)))
     (= 'qwerty/results (first lv))
     (let [[_ values app body] lv]
       (lower
        `(qwerty/results ~values ~app
                         ~(lower `(qwerty/set! ~f ~body)))))
     (or (= 'qwerty/return (first lv))
         (= 'qwerty/labels (first lv)))
     (lower
      `(qwerty/do
         (qwerty/set! ~f nil)
         ~lv))
     (= 'qwerty/go<- (first lv))
     (let [[_ [name channel] body] lv]
       (lower
        `(qwerty/go<- (~name ~channel)
                      ~(lower `(qwerty/set! ~f ~body)))))
     (= 'qwerty/go-> (first lv))
     (let [[_ [channel value] body] lv]
       (lower
        `(qwerty/go<- (~channel ~value)
                      ~(lower `(qwerty/set! ~f ~body)))))
     (or (= 'qwerty/make (first lv))
         (= 'qwerty/cast (first lv)))
     `(qwerty/set! ~f ~(lower lv))
     :else
     (do
       (assert (not (and (symbol? (first lv))
                         (= "qwerty" (namespace (first lv)))))
               lv)
       (let [bindings (for [x lv]
                        (if (symbol? x)
                          (list x x)
                          (list (gensym 'setbang) (lower x))))
             result (gensym 'result)]
         (lower
          `(qwerty/let* (~@(remove #(= (first %) (second %)) bindings)
                         (~result ~(lower (map first bindings))))
                        (qwerty/set! f ~result)))))
     ;; :else
     ;; (let [ff (gensym 'f)]
     ;;   (lower `(qwerty/let* ((~ff ~(lower lv)))
     ;;                        (qwerty/set! ~f ~ff))))
     :else
     (assert false (pr-str lv)))))

(defmethod lower-seq 'qwerty/+ [[_ a b]]
  (if (or (coll? a)
          (coll? b))
    (let [a_ (gensym 'a)
          b_ (gensym 'b)]
      (lower `(qwerty/let* ((~a_ ~(lower a))
                            (~b_ ~(lower b)))
                           (qwerty/+ ~a_ ~b_))))
    `(qwerty/+ ~a ~b)))

(defmethod lower-seq 'qwerty/bit-and [[_ a b]]
  (if (or (coll? a)
          (coll? b))
    (let [a_ (gensym 'a)
          b_ (gensym 'b)]
      (lower `(qwerty/let* ((~a_ ~(lower a))
                            (~b_ ~(lower b)))
                           (qwerty/bit-and ~a_ ~b_))))
    `(qwerty/bit-and ~a ~b)))

(defmethod lower-seq 'qwerty/- [[_ a b]]
  (if (or (coll? a)
          (coll? b))
    (let [a_ (gensym 'a)
          b_ (gensym 'b)]
      (lower `(qwerty/let* ((~a_ ~(lower a))
                            (~b_ ~(lower b)))
                           (qwerty/- ~a_ ~b_))))
    `(qwerty/- ~a ~b)))

(defmethod lower-seq 'qwerty/* [[_ a b]]
  (if (or (coll? a)
          (coll? b))
    (let [a_ (gensym 'a)
          b_ (gensym 'b)]
      (lower `(qwerty/let* ((~a_ ~(lower a))
                            (~b_ ~(lower b)))
                           (qwerty/* ~a_ ~b_))))
    `(qwerty/* ~a ~b)))

(defmethod lower-seq 'qwerty/< [[_ a b]]
  (if (or (coll? a)
          (coll? b))
    (let [a_ (gensym 'a)
          b_ (gensym 'b)]
      (lower `(qwerty/let* ((~a_ ~(lower a))
                            (~b_ ~(lower b)))
                           (qwerty/< ~a_ ~b_))))
    `(qwerty/< ~a ~b)))

(defmethod lower-seq 'qwerty/aget [[_ a b]]
  (if (or (not (symbol? a))
          (not (symbol? b)))
    (let [a_ (gensym 'a)
          b_ (gensym 'b)]
      (lower `(qwerty/let* ((~a_ ~(lower a))
                            (~b_ ~(lower b)))
                           (qwerty/aget ~a_ ~b_))))
    `(qwerty/aget ~a ~b)))

(defmethod lower-seq 'qwerty/nth* [[_ a b]]
  (if (or (coll? a)
          (coll? b))
    (let [a_ (gensym 'a)
          b_ (gensym 'b)]
      (lower `(qwerty/let* ((~a_ ~(lower a))
                            (~b_ ~(lower b)))
                           (qwerty/nth* ~a_ ~b_))))
    `(qwerty/nth* ~a ~b)))

(defmethod lower-seq 'qwerty/godef [[_ n v]]
  (cond
   (and (seq? v)
        (= 'qwerty/make (first v)))
   `(qwerty/godef ~n ~v)
   (coll? v)
   (let [a_ (gensym 'godef)]
     (lower
      `(qwerty/let* ((~a_ ~(lower v)))
                    (qwerty/do
                      (qwerty/local ~n ~'interface)
                      (qwerty/set! ~n ~a_)))))
   :else
   (lower
    `(qwerty/do
       (qwerty/local ~n ~(if (and (seq? v)
                                  (= 'qwerty/cast (first v)))
                           (second v)
                           'interface))
       (qwerty/set! ~n ~(lower v))))))

;;complicated
(defmethod lower-seq 'qwerty/results [[_ values application body]]
  (assert (seq? application) (pr-str `(qwerty/results ~values ~application ~body)))
  (cond
   (not (and (seq? body)
             (= 'qwerty/do (first body))
             (seq? (second body))
             (= 'qwerty/local (first (second body)))
             (= (first values) (second (second body)))))
   (lower
    `(qwerty/results ~values ~application
                     (qwerty/do
                       ~@(for [n values]
                           `(qwerty/local ~n ~'interface))
                       ~body)))
   (= (first application) 'qwerty/go-method-call)
   `(qwerty/results ~values ~application ~(lower body))
   (= (first application) 'qwerty/.)
   `(qwerty/results ~values ~(lower application) ~(lower body))
   (= (first application) 'qwerty/cast)
   `(qwerty/results ~values ~(lower application) ~(lower body))
   (= (first application) 'qwerty/map-entry)
   (let [[_ m k] application]
     (if (and (or (not (coll? m))
                  (= 'qwerty/goref (first m)))
              (or (not (coll? k))
                  (= 'qwerty/goref (first k))))
       `(qwerty/results ~values ~application ~(lower body))
       (let [ke (gensym 'k)
             ma (gensym 'm)]
         (lower `(qwerty/let* ((~ma ~(lower m))
                               (~ke ~(lower k)))
                              (qwerty/results ~values (qwerty/map-entry ~ma ~ke) ~(lower body)))))))
   (every? (complement coll?) application)
   (let [f (gensym 'f)]
     (lower
      `(qwerty/let* ((~f (qwerty/cast ~(fn-interface) ~(first application))))
                    (qwerty/results
                     ~values
                     (qwerty/go-method-call ~f ~(symbol (str "Invoke" (count (rest application)) "_"
                                                             (count values)))
                                            ~@(rest application))
                     ~(lower body)))))
   :else
   (do
     (assert (or (not (symbol? (first application)))
                 (not= "qwerty" (namespace (first application)))))
     (let [bindings (for [e application]
                      (if (symbol? e)
                        (list e e)
                        (list (gensym 'r) (lower e))))]
       (lower
        `(qwerty/let* ~(remove #(= (first %) (second %)) bindings)
                      (qwerty/results ~values ~(map first bindings)
                                      ~(lower body))))))))

;; (defmethod lower-seq 'qwerty/do [[_ & body]]
;;   `(qwerty/do ~@(map lower body)))

(defmethod lower-seq 'qwerty/type [exp]
  exp)

(defmethod lower-seq 'qwerty/definterface [exp]
  exp)

(defmethod lower-seq 'qwerty/make [[_ t & args]]
  (if (some coll? args)
    (let [args (for [a args]
                 (if (symbol? a)
                   (list a a)
                   (list (gensym 'a) a)))]
      (lower
       `(qwerty/let* ~(remove #(= (first %) (second %)) args)
                     (qwerty/make ~t ~@(map first args)))))
    `(qwerty/make ~t ~@args)))

(defmethod lower-seq 'qwerty/goto [exp]
  exp)

(defmethod lower-seq 'qwerty/labels [[_ & body]]
  `(qwerty/labels
    ~@(for [e body]
        (if (symbol? e)
          e
          (lower e)))))

(defmethod lower-seq 'qwerty/go-method-call [[_ target method & args]]
  (if (and (not (coll? target))
           (every? (complement coll?) args))
    `(qwerty/go-method-call ~target ~method ~@args)
    (let [bindings (for [a (cons target args)]
                     (if (symbol? a)
                       (list a a)
                       (list (gensym 'a) (lower a))))]
      (lower
       `(qwerty/let* ~(remove #(= (first %) (second %)) bindings)
                     (qwerty/go-method-call ~(first (first bindings)) ~method ~@(map first (rest bindings))))))))

(defmethod lower-seq 'qwerty/if [[_ cond then else]]
  (let [e-l (gensym 'else)
        end (gensym 'end)
        phi (gensym 'phi)
        c (gensym 'condition)]
    (lower
     `(qwerty/let* ((~c ~(lower cond)))
                   (qwerty/do
                     (qwerty/local ~phi ~'interface)
                     (qwerty/labels
                      (qwerty/test ~c ~e-l)
                      ~(lower
                        `(qwerty/do
                           (qwerty/set! ~phi ~(lower then))
                           (qwerty/goto ~end)))
                      ~e-l
                      ~(lower
                        `(qwerty/do
                           (qwerty/set! ~phi ~(lower else))
                           (qwerty/goto ~end)))
                      ~end)
                     ~phi)))))

(defmethod lower-seq 'qwerty/test [[_ condition label]]
  (if (coll? condition)
    (let [c (gensym 'c)]
      (lower `(qwerty/let* ((~c ~(lower condition)))
                           (qwerty/test ~c ~label))))
    `(qwerty/test ~condition ~label)))

(defmethod lower-seq 'qwerty/go-> [[_ [value channel] body]]
  (cond
   (and (not (coll? value))
        (not (coll? channel)))
   `(qwerty/go-> (~value ~channel) ~(lower body))
   :else
   (assert false)))

(defmethod lower-seq 'qwerty/go<- [exp]
  ;; (binding [*out* *err*]
  ;;   (pprint exp)
  ;;   (println))
  (let [[_ [result channel] body] exp]
    (cond
     (and (symbol? result)
          (symbol? channel))
     `(qwerty/go<- (~result ~channel)
                   ~(if (not (and (seq? body)
                                  (= 'qwerty/do (first body))
                                  (seq? (second body))
                                  (= 'qwerty/local (first (second body)))
                                  (= result (second (second body)))))
                      `(qwerty/do
                         (qwerty/local ~result ~'interface)
                         ~(lower body))
                      (lower body)))
     (symbol? result)
     (let [c (gensym 'channel)]
       (lower
        `(qwerty/let* ((~c ~channel))
                      (qwerty/go<- (~result ~c)
                                   ~(if (not (and (seq? body)
                                                  (= 'qwerty/do (first body))
                                                  (seq? (second body))
                                                  (= 'qwerty/local (first (second body)))
                                                  (= result (second (second body)))))
                                      `(qwerty/do
                                         (qwerty/local ~result ~'interface)
                                         ~(lower body))
                                      (lower body))))))
     :else
     (assert false))))

(defmethod lower-seq 'qwerty/go [[_ fun]]
  (if (symbol? fun)
    `(qwerty/go ~fun)
    (let [go (gensym 'go)]
      (lower
       `(qwerty/let* ((~go ~(lower fun)))
                     (qwerty/go ~go))))))

(defmethod lower-seq 'qwerty/map-update [[_ & args]]
  (if (every? #(or (symbol? %)
                   (and (seq? %)
                        (= 'qwerty/goref (first %)))) args)
    `(qwerty/map-update ~@args)
    (let [bindings (for [a args]
                     (if (or (symbol? a)
                             (and (seq? a)
                                  (= 'qwerty/goref (first a))))
                       (list a a)
                       (list (gensym 'mapu) (lower a))))]
      (lower
       `(qwerty/let* ~(remove #(= (first %) (second %)) bindings)
                     (qwerty/map-update ~@(map first bindings)))))))

(defmethod lower-seq 'qwerty/= [[_ a b]]
  (cond
   (and (symbol? a)
        (symbol? b))
   `(qwerty/= ~a ~b)
   (symbol? a)
   (let [bb (gensym 'b)]
     (lower
      `(qwerty/let* ((~bb ~b))
                    (qwerty/= ~a ~bb))))
   (symbol? b)
   (let [aa (gensym 'a)]
     (lower
      `(qwerty/let* ((~aa ~a))
                    (qwerty/= ~aa ~b))))
   :else
   (let [aa (gensym 'a)
         bb (gensym 'b)]
     (lower
      `(qwerty/let* ((~aa ~a)
                     (~bb ~b))
                    (qwerty/= ~aa ~bb))))))

(defmethod lower-seq 'qwerty/goref [exp]
  exp)

(defmethod lower-seq 'qwerty/defer [[_ v]]
  (if (symbol? v)
    `(qwerty/defer ~v)
    (let [x (gensym 'defer)]
      (lower
       `(qwerty/let* ((~x ~(lower v))) (qwerty/defer ~x))))))

(defmethod lower-seq 'qwerty/def [[_ n v]]
  (swap! global-env conj n)
  (lower
   `(~'qwerty/.
     ~(if (= *package* 'qwerty)
        'InternVar_
        'qwerty.InternVar_)
     (qwerty/quote ~n)
     ~(lower v))))

(defmethod lower-seq 'qwerty/quote [[_ v]]
  (cond
   (symbol? v)
   (if (= *package* 'qwerty)
     `(qwerty/. ~'Symbol_ ~(str v))
     `(qwerty/. ~'qwerty.Symbol_ ~(str v)))
   (seq? v)
   (if (= *package* 'qwerty)
     (let [bindings (for [item v]
                      (list (gensym 'quote) (lower `(qwerty/quote ~item))))]
       (lower
        `(qwerty/let* ~bindings
                      ~(reduce
                        (fn [tail [el _]]
                          `(qwerty/. ~'Cons ~el ~tail))
                        nil (reverse bindings)))))
     (let [bindings (for [item v]
                      (list (gensym 'quote) (lower `(qwerty/quote ~item))))]
       (lower
        `(qwerty/let* ~bindings
                      ~(reduce
                        (fn [tail [el _]]
                          `(qwerty/. ~'qwerty.Cons ~el ~tail))
                        nil (reverse bindings))))))
   (number? v)
   v
   (or (nil? v) (empty? v))
   nil
   (string? v)
   v
   :else (assert false)))

(defmethod lower-seq :default [form]
  (assert (not (nil? form)) form)
  (assert (not (nil? (first form))) (pr-str form))
  (assert (or (coll? (first form))
              (not= "qwerty" (namespace (first form)))) (with-out-str (pprint form)))
  (if (not (every? symbol? form))
    (let [lowered (map lower form)
          bindings (for [l lowered]
                     (if (symbol? l)
                       (list l l)
                       (list (gensym 'arg) l)))]
      (lower `(qwerty/let* (~@(remove (fn [[n v]] (= n v)) bindings))
                           (qwerty/do
                             (qwerty/comment "line" ~(:line (meta form)))
                             ~(lower (map first bindings))))))
    (let [f (gensym 'f)]
      (lower `(qwerty/let* ((~f (qwerty/cast ~(fn-interface) ~(first form))))
                           (qwerty/do
                             (qwerty/comment "line" ~(:line (meta form)))
                             (qwerty/go-method-call ~f ~(symbol (str "Invoke" (count (rest form)) "_1"))
                                                    ~@(rest form))))))))

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

(def info (atom {}))

(defmulti go type)

(defmulti go-seq first)

(defmethod go clojure.lang.ISeq [s]
  (go-seq s))

(defmethod go java.lang.String [s]
  (if (= :return *context*)
    (print "return"))
  (print " ")
  (pr s)
  (print " ")
  (if (= :return *context*)
    (println)))

(defmethod go java.lang.Long [s]
  (if (= :return *context*)
    (print "return"))
  (print " ")
  (pr s)
  (print " ")
  (if (= :return *context*)
    (println)))

(defmethod go java.lang.Boolean [s]
  (if (= :return *context*)
    (print "return"))
  (print " ")
  (pr s)
  (print " ")
  (if (= :return *context*)
    (println)))

(defmethod go java.lang.Character [s]
  #_(binding [*out* *err*]
      (prn (type s))
      (prn s))
  (if (= :return *context*)
    (print "return"))
  (print "rune(" (int s) ")")
  (if (= :return *context*)
    (println)))

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
  (println "/* qwerty/do */")
  (when (> (count s) 1)
    (doseq [item (butlast (rest s))]
      (binding [*context* :statement]
        (go item)
        (println)))
    (go (last s))))

(defmethod go-seq 'qwerty/func [[_ name-or-target-type :as exp]]
  (binding [*scope* :function
            *context* :statement]
    (if (symbol? name-or-target-type)
      (let [[_ name args returns body] exp
            args (for [[T n type] args]
                   (do
                     (assert (= T 'qwerty/T))
                     (str n " " (type-string type))))
            returns (for [[T n type] returns]
                      (do
                        (assert (= T 'qwerty/T))
                        (type-string type)))]
        (println "func" name "("(apply str (interpose \, args)) ")"
                 "("(apply str (interpose \, returns)) ")"
                 "{")
        (go body)
        (println "}"))
      (let [[_ target name args returns body] exp
            args (for [[T n type] args]
                   (do
                     (assert (= T 'qwerty/T))
                     (str n " " (type-string type))))
            returns (for [[T n type] returns]
                      (do
                        (assert (= T 'qwerty/T))
                        (type-string type)))
            [T n type] target
            _ (assert (= T 'qwerty/T) T)
            target (str n " " (type-string type))]
        (println "func" "(" target ")" name "("(apply str (interpose \, args)) ")"
                 "("(apply str (interpose \, returns)) ")"
                 "{")
        (go body)
        (println "}")))))

(defmethod go-seq 'qwerty/return [[_ v]]
  (println)
  (print "return ")
  (go v)
  (println))

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
  (let [fields (filter seq? fields)
        structs (remove seq? fields)]
    (doseq [struct structs]
      (println (type-string struct)))
    (doseq [[T name type] fields]
      (assert (= 'qwerty/T T))
      (println " " name (type-string type) " ")))
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

(defmethod go-seq 'qwerty/make [[_ type-exp & args]]
  (if (= :return *context*)
    (print "return"))
  (print "make(" (type-string type-exp))
  (when (seq args)
    (print ",")
    (doseq [a args]
      (print a)
      ","))
  (print  ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/map-entry [[_ m k]]
  (if (= :return *context*)
    (print "return"))
  (print "" (str (if (and (seq? m)
                          (= 'qwerty/goref (first m)))
                   (second m)
                   (munge m))  "[" k "]"))
  (if (= :return *context*)
    (println)))


(defmethod go-seq 'qwerty/aget [[_ m k]]
  (if (= :return *context*)
    (print "return"))
  (print "" (str (if (and (seq? m)
                          (= 'qwerty/goref (first m)))
                   (second m)
                   (munge m))
                 "["))
  (binding [*context* :statement]
    (go k))
  (print "]")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/go-method-call [[_ target method & args]]
  (if (= :return *context*)
    (print "return"))
  (binding [*context* :statement]
    (go target))
  (print (str "." method "("))
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
  (binding [*context* :statement]
    (go n))
  (print ".(" (type-string t) ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/+ [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (print "(" a "+" b ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/bit-and [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (print "(" a "&" b ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/< [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (print "(" a "<" b ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/- [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (print "(" a "-" b ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/* [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (print "(" a "*" b ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/= [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (print "(" a "==" b ")")
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/nth* [[_ a b]]
  (if (= :return *context*)
    (print "return"))
  (binding [*context* :statement]
    (go a)
    (print "[")
    (go b)
    (print "]"))
  (if (= :return *context*)
    (println)))

(defmethod go-seq 'qwerty/go-> [[_ [value channel] body]]
  (let [channel-type (get @info channel)]
    (print (str channel ".(" (if (= channel-type "interface{}") "chan interface{}" channel-type) ")") "<- "))
  (binding [*context* :statement]
    (go value))
  (println)
  (go body))

(defmethod go-seq 'qwerty/go<- [[_ [result channel] body]]
  (assert (not (coll? result)))
  (if (and (seq? body)
           (= 'qwerty/do (first body))
           (seq? (second body))
           (= 'qwerty/local (first (second body)))
           (= result (second (second body))))
    (let [local (second body)
          body (cons 'qwerty/do (rest (rest body)))]
      (go local)
      (println result "= <-"(str channel ".(chan interface{})"))
      (println)
      (go body))
    (do
      (println result "= <-"(str channel ".(chan interface{})"))
      (println)
      (go body))))


(defmethod go-seq 'qwerty/values [[_  & args]]
  ;; (assert (= *context* :return))
  (println "/* qwerty/values */")
  (println "return" (apply str (interpose \, (for [a args]
                                               (if (nil? a)
                                                 "nil"
                                                 a))))))

(defmethod go-seq 'qwerty/labels [[_  & e]]
  (println "/* qwerty/labels */")
  (doseq [e e]
    (if (symbol? e)
      (println (str "L" e ":"))
      (do
        (go e)
        (println)))))

(defmethod go-seq 'qwerty/go [[_  fun]]
  (println "go" (str "(" fun ".(" (fn-interface) ")).Invoke0_1()")))

(defmethod go-seq 'qwerty/defer [[_  fun]]
  (println "defer" (str "(" fun ".(" (fn-interface) ")).Invoke0_1()")))

(defmethod go-seq 'qwerty/test [[_ condition label]]
  (println "if" (str "!(" condition ".(bool))") "{ goto" (str "L" label) "}"))

(defmethod go-seq 'qwerty/goto [[_ label]]
  (println "goto" (str "L" label)))

(defmethod go-seq 'qwerty/results [[_  names exp body]]
  (print (apply str (interpose \, (map munge names))) "=")
  (binding [*context* :statement]
    (go exp))
  (println)
  (go body))

(defmethod go-seq 'qwerty/definterface [[_ interface-name & methods]]
  (println "type" interface-name "interface {")
  (doseq [[name args returns] methods]
    (println name "(" (apply str (interpose \, (repeat (count args) "interface{}"))) ")"
             "(" (apply str (interpose \, (repeat (count returns) "interface{}"))) ")"))
  (println "}"))

(defmethod go-seq 'qwerty/comment [[_ & args]]
  (println "/*" (apply print-str args) "*/"))

(defmethod go-seq 'qwerty/local [[_ n type]]
  (println "/* qwerty/local" n type "*/")
  (swap! info assoc n (if (= 'interface type)
                        "interface{}"
                        type))
  (println "var" (munge n) (if (= 'interface type)
                             "interface{}"
                             type)))

(defmethod go-seq 'qwerty/nil? [[_ v]]
  (print "/* qwerty/nil? */")
  (println "(" v "== nil" ")"))

(defmethod go-seq 'qwerty/godef [[_ n v]]
  (assert  (and (seq? v)
                (= 'qwerty/make (first v)))
           (pr-str n v))
  (print "var" n (type-string (second v))  "= ")
  (go v)
  (println))

(defmethod go-seq 'qwerty/map-update [[_ map key value]]
  (binding [*context* :statement]
    (print "/* qwerty/nil? */")
    (go map)
    (print "[")
    (go key)
    (print "] =")
    (go value)
    (println)))

(defmethod go-seq 'qwerty/goref [[_ v]]
  (go v))

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
           qwerty/defgomethod
           qwerty/definterface
           qwerty/godef
           qwerty/func
           } (first form))))

(defmethod raise-decls-seq 'qwerty/func [[_ name-or-target-type :as exp]]
  (if (symbol? name-or-target-type)
    (let [[_ name args returns body] exp
          body (if (seq? body) body `(qwerty/do ~body))
          defs (doall (filter decl? body))
          body (doall (map raise-decls (remove decl? body)))]
      `(qwerty/do
         ~@defs
         (qwerty/func ~name ~args ~returns ~body)))
    (let [[_ target name args returns body] exp
          body (if (seq? body) body `(qwerty/do ~body))
          defs (doall (filter decl? body))
          body (doall (map raise-decls (remove decl? body)))]
      `(qwerty/do
         ~@defs
         (qwerty/func ~target ~name ~args ~returns ~body)))))

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

(defmethod raise-decls-seq 'qwerty/labels [[_ & body]]
  (let [raised-bindings (for [exp body]
                          (if (and (seq? exp)
                                   (= 'qwerty/do (first exp)))
                            (let [decls (filter decl? (rest exp))
                                  exps (remove decl? (rest exp))]
                              {:exp `(qwerty/do ~@exps)
                               :decls decls})
                            {:exp exp
                             :decls []}))]
    `(qwerty/do
       ~@(mapcat :decls raised-bindings)
       (qwerty/labels
        ~@(map :exp raised-bindings)))))

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

(declare raise-locals-out-of-labels)
(defmulti raise-locals (fn [exp up-env down-env] (type exp)))
(defmulti raise-locals-seq (fn [exp up-env down-env] (first exp)))
(defmethod raise-locals clojure.lang.ISeq [exp up-env down-env]
  (raise-locals-seq exp up-env down-env))
(defmethod raise-locals clojure.lang.Symbol [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals java.lang.String [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals java.lang.Number [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals java.lang.Character [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals java.lang.Boolean [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals nil [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/struct [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/definterface [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/do [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/local [local up-env down-env]
  [(if (contains? down-env local) `
     (qwerty/do)
     local)
   up-env
   down-env])
(defmethod raise-locals-seq 'qwerty/set! [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/make [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/. [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/values [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/comment [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/func [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/return [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/new [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/cast [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/.- [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/results [[_ values app body] up-env down-env]
  [(if (and (seq? body)
            (= 'qwerty/do (first body))
            (some #(and (seq? %) (= 'qwerty/local (first %))) (rest body)))
     `(qwerty/do
        ~@(filter #(and (seq? %) (= 'qwerty/local (first %))) (rest body))
        (qwerty/results ~values ~app
                        (qwerty/do
                          ~@(remove #(and (seq? %) (= 'qwerty/local (first %))) (rest body)))))
     `(qwerty/results ~values ~app ~body))
   up-env
   down-env])
(defmethod raise-locals-seq 'qwerty/labels [[_ & exps] up-env down-env]
  (let [x (for [e exps]
            (if (symbol? e)
              [#{} e]
              (let [locals (filter #(and (seq? %)
                                         (= 'qwerty/local (first %)))
                                   (tree-seq seq? seq e))]
                [(set locals) (raise-locals-out-of-labels e (into down-env locals))])))]
    (if (not (empty? (mapcat first x)))
      [`(qwerty/do
          ~@(doall (distinct (mapcat first x)))
          (qwerty/labels ~@(doall (map second x))))
       up-env
       down-env]
      [`(qwerty/labels ~@exps)
       up-env
       down-env])))
(defmethod raise-locals-seq 'qwerty/go-method-call [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/+ [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/- [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/* [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/goderef [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/nil? [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/test [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/goto [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/go-> [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/go<- [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/go [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/= [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/let* [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/map-update [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/godef [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/nth* [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/goref [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/defer [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/< [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/aget [exp up-env down-env] [exp up-env down-env])
(defmethod raise-locals-seq 'qwerty/bit-and [exp up-env down-env] [exp up-env down-env])

(defn raise-locals-out-of-labels [form seen]
  (first (expand form {} seen raise-locals)))

(defn f [form]
  ;; (binding [*out* *err*]
  ;;   (pprint form)
  ;;   (println))
  (let [nf (raise-locals-out-of-labels (collapse-do (raise-decls form)) #{})]
    (if (= nf form)
      form
      (recur nf))))


(def init-n (atom -1))


(def init-name (gensym 'init))

(defn init-fn-name []
  (swap! init-n inc)
  (symbol (str init-name @init-n)))

(defn init-fun []
  (lower
   `(qwerty/func ~'init () ()
                 (qwerty/do
                   ~@(for [n (range (inc @init-n))]
                       `(qwerty/. ~(symbol (str init-name n))))))))

(defn top-level-init [[op & exprs]]
  (if (= 'qwerty/do op)
    (let [decls (filter #(or (decl? %)
                             (and (seq? %)
                                  (= 'qwerty/local (first %)))) exprs)
          effects (remove #(or (decl? %)
                               (and (seq? %)
                                    (= 'qwerty/local (first %)))) exprs)]
      `(qwerty/do
         ~@decls
         ~@(when (seq effects)
             [`(qwerty/func ~(init-fn-name) () ()
                            (qwerty/do
                              ~@effects))])))
    `(~op ~@exprs)))

(try
  (let [eof (Object.)]
    (with-open [i (-> (io/reader "compilation-env") java.io.PushbackReader.)]
      (loop [name (read i false eof)]
        (when-not (= eof name)
          (swap! global-env conj name)
          (recur (read i false eof))))))
  (binding [*out* *err*]
    (prn @global-env))
  (catch Exception e
    (binding [*out* *err*]
      (prn e))))

(def varize? (atom true))

(let [eof (Object.)]
  (with-open [ir (if (System/getenv "IR")
                   (io/writer "ir.q")
                   (io/writer "/dev/null"))]
    (binding [*package* nil]
      (try
        (loop [form (read *in* false eof)]
          (when-not (= eof form)
            (cond
             (and (seq? form) (= (first form) 'qwerty/package))
             (do
               (set! *package* (second form))
               (println "package " (second form))
               (when (not= *package* 'qwerty)
                 (println "import " (pr-str "qwerty/lisp"))))
             (and (seq? form) (= (first form) 'qwerty/import))
             (println "import " (pr-str (str (second form))))
             (and (seq? form) (= (first form) 'qwerty/varize))
             (reset! varize? (second form))
             :else (let [[new-form up-env down-env] (if @varize?
                                                      (varize form {} {})
                                                      [form {} {}])
                         new-form (if (seq (:vars up-env))
                                    `(qwerty/let* ~(for [[k v] (:vars up-env)]
                                                     (list k (if (= *package* 'qwerty)
                                                               `(qwerty/. ~'Var_ (qwerty/quote ~v))
                                                               `(qwerty/. ~'qwerty.Var_ (qwerty/quote ~v)))))
                                                  ~new-form)
                                    new-form)
                         ;; _ (binding [*out* *err*]
                         ;;     (pprint new-form)
                         ;;     (println))
                         m (top-level-init (f (lower (α-convert new-form {}))))]
                     (when (System/getenv "IR")
                       (binding [*out* ir
                                 *print-right-margin* 120]
                         (pprint m)
                         (println)
                         (println)))
                     (go m)))
            (println)
            (recur (read *in* false eof))))
        (go (init-fun))
        (finally
          (with-open [o (io/writer "compilation-env")]
            (binding [*out* o]
              (doseq [e @global-env]
                (println e)))))))))



(comment

  ;; state for interpreter

  {:vtables {type methods}
   :stack [{name {:value value
                  :type type}}]
   :globals {}
   :args []
   :returns []}


  )
