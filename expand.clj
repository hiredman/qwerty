
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

(defmethod children-of java.lang.Character [s] ())
(defmethod make java.lang.Character [form new-children]
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

(defmethod children-of-seq 'qwerty/defgomethod [[_ method-name type-name args returns types body]]
  (list body))
(defmethod make-seq 'qwerty/defgomethod [[_ method-name type-name args returns types body] new-children]
  (assert (= 1 (count new-children)) (pr-str new-children))
  `(qwerty/defgomethod ~method-name ~type-name ~args ~returns ~types ~(first new-children)))

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

(defmethod children-of-seq 'qwerty/- [[_ & args]]
  args)
(defmethod make-seq 'qwerty/- [[_ a b] new-children]
  (assert (= 2 (count new-children)))
  `(qwerty/- ~@new-children))

(defmethod children-of-seq 'qwerty/* [[_ & args]]
  args)
(defmethod make-seq 'qwerty/* [[_ a b] new-children]
  (assert (= 2 (count new-children)))
  `(qwerty/* ~@new-children))

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

(defmethod children-of-seq 'qwerty/nth* [[_ a b]]
  (list a b))
(defmethod make-seq 'qwerty/nth* [_ new-children]
  (assert (= 2 (count new-children)))
  `(qwerty/nth* ~@new-children))

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

(defmethod children-of-seq 'qwerty/let* [[_ bindings body]]
  (list body))
(defmethod make-seq 'qwerty/let* [[_ bindings body] new-children]
  (assert (= 1 (count new-children)))
  (assert (every? #(and (seq? %) (= 2 (count %))) bindings))
  (doall `(qwerty/let* ~bindings ~@new-children)))

(defmethod children-of-seq 'qwerty/if [[_ & parts]]
  parts)
(defmethod make-seq 'qwerty/if [[_ & parts] new-children]
  (assert (> 4 (count new-children)))
  `(qwerty/if ~@new-children))


(defmethod children-of-seq 'qwerty/go-> [[_ [value channel] body]]
  (list value channel body))
(defmethod make-seq 'qwerty/go-> [[_ [value channel] body] new-children]
  (assert (= 3 (count new-children)))
  `(qwerty/go-> (~(first new-children) ~(second new-children)) ~@(rest (rest new-children))))

(defmethod children-of-seq 'qwerty/go<- [[_ [binding channel] body]]
  (list channel body))
(defmethod make-seq 'qwerty/go<- [[_ [binding channel] body] new-children]
  (assert (= 2 (count new-children)))
  `(qwerty/go<- (~binding ~(first new-children)) ~@(rest new-children)))

(defmethod children-of-seq 'qwerty/go [[_ fun]]
  (list fun))
(defmethod make-seq 'qwerty/go [[_ fun] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/go ~@new-children))

(defmethod children-of-seq 'qwerty/fn* [[_ args body]]
  (list body))
(defmethod make-seq 'qwerty/fn* [[_ args body] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/fn* ~args ~@new-children))

(defmethod children-of-seq 'qwerty/= [[_ & args]]
  args)
(defmethod make-seq 'qwerty/= [_ new-children]
  (assert (= 2 (count new-children)))
  `(qwerty/= ~@new-children))

(defmethod children-of-seq 'qwerty/map-update [[_ & args]]
  args)
(defmethod make-seq 'qwerty/map-update [_ new-children]
  (assert (= 3 (count new-children)))
  `(qwerty/map-update ~@new-children))

(defmethod children-of-seq 'qwerty/quote [_] ())
(defmethod make-seq 'qwerty/quote [exp new-children]
  (assert (zero? (count new-children)))
  exp)

(defmethod children-of-seq 'qwerty/godef [[_ _ v]] (list v))
(defmethod make-seq 'qwerty/godef [[_ n _] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/godef ~n ~@new-children))

(defmethod children-of-seq 'qwerty/def [[_ n v]]
  (swap! global-env conj n)
  (list v))
(defmethod make-seq 'qwerty/def [[_ n _] new-children]
  (assert (= 1 (count new-children)))
  `(qwerty/def ~n ~@new-children))

(defmethod children-of-seq 'qwerty/goref [_] ())
(defmethod make-seq 'qwerty/goref [[_ v] new-children]
  (assert (= 0 (count new-children)))
  `(qwerty/goref ~v))

(defmethod children-of-seq :default [exp]
  (assert (not (and (symbol? (first exp))
                    (= "qwerty" (namespace (first exp)))))
          (pr-str exp))
  exp)
(defmethod make-seq :default [exp new-children]
  (assert (not (and (symbol? (first exp))
                    (= "qwerty" (namespace (first exp)))))
          (pr-str exp))
  new-children)

(defn expand [form env f]
  (assert (not (vector? form)) (pr-str env))
  (loop [form form
         env env]
    (let [[new-form new-env] (f form env)
          new-form (if (instance? clojure.lang.IMeta new-form)
                     (with-meta new-form (meta form))
                     new-form)
          x form]
      (if (= new-form form)
        (let [[new-env new-children] (reduce
                                      (fn [[env children] form]
                                        (assert (not (vector? form))
                                                (pr-str form new-form x (children-of new-form)))
                                        (let [[new-form new-env] (expand form env f)]
                                          [new-env (conj children new-form)]))
                                      [new-env []]
                                      (children-of new-form))]
          [(make new-form (seq new-children)) new-env])
        (recur new-form new-env)))))
