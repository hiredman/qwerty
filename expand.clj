(defmulti make (fn [form new-children] (type form)))
(defmulti make-seq (fn [form new-children] (first form)))

(defmethod make clojure.lang.ISeq [s new-children]
  (make-seq s new-children))

(defmulti children-of type)
(defmulti children-of-seq first)

(defmethod children-of clojure.lang.Symbol [s] ())
(defmethod make clojure.lang.Symbol [form new-children]
  (assert (empty? new-children))
  form)

(defmethod children-of java.lang.String [s] ())
(defmethod make java.lang.String [form new-children]
  (assert (empty? new-children))
  form)

(defmethod children-of java.lang.Number [s] ())
(defmethod make java.lang.Number [form new-children]
  (assert (empty? new-children))
  form)

(defmethod children-of nil [s] ())
(defmethod make nil [form new-children]
  (assert (empty? new-children))
  form)

(defmethod children-of clojure.lang.ISeq [s]
  (children-of-seq s))

(defmethod children-of-seq 'qwerty/struct [form] ())
(defmethod make-seq 'qwerty/struct [form new-children]
  (assert (empty? new-children) (pr-str new-children))
  form)

(defmethod children-of-seq 'qwerty/definterface [form] ())
(defmethod make-seq 'qwerty/definterface [form new-children]
  (assert (empty? new-children) (pr-str new-children))
  form)

(defmethod children-of-seq 'qwerty/do [[_ & children]] children)
(defmethod make-seq 'qwerty/do [form new-children]
  `(qwerty/do ~@new-children))

(defmethod children-of-seq 'qwerty/local [_] ())
(defmethod make-seq 'qwerty/local[form new-children]
  (assert (empty? new-children) (pr-str new-children))
  form)


(defmethod children-of-seq 'qwerty/local [_] ())
(defmethod make-seq 'qwerty/local[form new-children]
  (assert (empty? new-children) (pr-str new-children))
  form)

(defmethod children-of-seq 'qwerty/set! [[_ _ s]] (list s))
(defmethod make-seq 'qwerty/set! [form new-children]
  (assert (= 1 (count new-children)) (pr-str new-children))
  `(qwerty/set! ~(second form) ~@new-children))

(defmethod children-of-seq 'qwerty/make [exp] ())
(defmethod make-seq 'qwerty/make [form new-children]
  (assert (empty? new-children) (pr-str new-children))
  form)

(defmethod children-of-seq 'qwerty/defgomethod [[_ method-name type-name args returns body]]
  (list body))
(defmethod make-seq 'qwerty/defgomethod [[_ method-name type-name args returns body] new-children]
  (assert (= 1 (count new-children)) (pr-str new-children))
  `(qwerty/defgomethod ~method-name ~type-name ~args ~returns ~(first new-children)))

(defmethod children-of-seq 'qwerty/. [[_ method-name & args]]
  args)
(defmethod make-seq 'qwerty/. [[_ method-name & args] new-children]
  `(qwerty/. ~method-name ~@new-children))

(defmethod children-of-seq 'qwerty/values [[_ & args]]
  args)
(defmethod make-seq 'qwerty/values [[_ & args] new-children]
  `(qwerty/values ~@new-children))

(defmethod children-of-seq 'qwerty/comment [[_ & args]]
  ())
(defmethod make-seq 'qwerty/comment [form new-children]
  (assert (empty? new-children))
  form)

(defmethod children-of-seq 'qwerty/defgofun [[_ function-name args types body]]
  (list body))
(defmethod make-seq 'qwerty/defgofun [[_ function-name args types body] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/defgofun ~function-name ~args ~types ~(first new-children)))

(defmethod children-of-seq 'qwerty/new [[_ & args]]
  ())
(defmethod make-seq 'qwerty/new [form new-children]
  (assert (empty? new-children))
  form)

(defmethod children-of-seq 'qwerty/cast [[_ t v]]
  (list v))
(defmethod make-seq 'qwerty/cast [[_ t v] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/cast ~t ~(first new-children)))

(defmethod children-of-seq 'qwerty/.- [[_ target field]]
  (list target))
(defmethod make-seq 'qwerty/.- [[_ t field] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/.- ~(first new-children) ~field))

(defmethod children-of-seq 'qwerty/results [[_ values app body]]
  (list body))
(defmethod make-seq 'qwerty/results [[_ values app body] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/results ~values ~app ~(first new-children)))

(defmethod children-of-seq 'qwerty/results [[_ values app body]]
  (list body))
(defmethod make-seq 'qwerty/results [[_ values app body] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/results ~values ~app ~(first new-children)))

(defmethod children-of-seq 'qwerty/labels [_]
  (list))
(defmethod make-seq 'qwerty/labels [form new-children]
  (assert (empty? new-children))
  form)

(defmethod children-of-seq 'qwerty/go-method-call [[_ target method-name & args]]
  (cons target args))
(defmethod make-seq 'qwerty/go-method-call [[_ target method-name & args] [new-target & new-args]]
  `(qwerty/go-method-call ~new-target ~method-name ~@new-args))

(defmethod children-of-seq 'qwerty/+ [[_ & args]]
  args)
(defmethod make-seq 'qwerty/+ [[_ a b] new-children]
  (assert (= 2 (count new-children)))
  `(qwerty/+ ~@new-children))

(defmethod children-of-seq 'qwerty/goderef [[_ v]]
  (list v))
(defmethod make-seq 'qwerty/goderef [[_ v] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/goderef ~@new-children))

(defmethod children-of-seq 'qwerty/nil? [[_ v]]
  (list v))
(defmethod make-seq 'qwerty/nil? [[_ v] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/nil? ~@new-children))

(defmethod children-of-seq 'qwerty/test [[_ c label]]
  (list c))
(defmethod make-seq 'qwerty/test [[_ c label] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/test ~@new-children ~label))

(defmethod children-of-seq 'qwerty/goto [[_ label]]
  (list))
(defmethod make-seq 'qwerty/goto [form new-children]
  (assert (empty? new-children))
  form)

(defn expand [form env f]
  (loop [form form
         env env]
    (let [[new-form new-env] (f form env)]
      (if true #_(= new-form form)
        (make new-form (for [child (children-of new-form)]
                         (expand child new-env f)))
        (recur new-form new-env)))))
