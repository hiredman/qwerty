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
  `(qwerty/. ~function ~@(doall (map #(α-convert % env) args))))

(defmethod α-convert-seq 'qwerty/godef [[_ n body] env]
  `(qwerty/godef ~n ~(α-convert body env)))

(defmethod α-convert-seq 'qwerty/defgomethod [[_ method-name type-name args returns types body] env]
  (let [new-env (for [a args]
                  [a (gensym a)])]
    `(qwerty/defgomethod ~method-name ~type-name ~(map second new-env) ~returns ~types
       ~(α-convert body (into env new-env)))))

(defmethod α-convert-seq 'qwerty/fn* [[_ args body] env]
  (let [args (for [a args]
               [a (gensym a)])]
    (with-meta `(qwerty/fn* ~(doall (map second args))
                            ~(α-convert body (into env args)))
      (meta env))))

(defmethod α-convert-seq 'qwerty/results [[_ values exp body] env]
  (let [args (for [a values]
               [a (gensym a)])]
    `(qwerty/results ~(doall (map second args))
                     ~(α-convert exp env)
                     ~(α-convert body (into env args)))))

(defmethod α-convert-seq 'qwerty/defgofun [[_ name args types body] env]
  (let [args (for [a args]
               [a (gensym a)])]
    (with-meta
      (doall
       `(qwerty/defgofun
          ~name
          ~(doall (map second args))
          ~types
          ~(α-convert body (into env args))))
      (meta env))))

(defmethod α-convert-seq 'qwerty/do [[_ & body] env]
  (doall `(qwerty/do ~@(doall (map #(α-convert % env) body)))))

(defmethod α-convert-seq 'qwerty/values [[_ & body] env]
  (doall `(qwerty/values ~@(doall (map #(α-convert % env) body)))))

(defmethod α-convert-seq 'qwerty/let* [[_ bindings body :as form] env]
  (let [{:keys [bindings env]} (reduce
                                (fn [{:keys [bindings env]} [n v]]
                                  (let [nn (gensym n)]
                                    {:bindings (conj bindings (list nn (α-convert v env)))
                                     :env (assoc env n nn)}))
                                {:bindings []
                                 :env env} bindings)]
    (with-meta
      (doall
       `(qwerty/let* ~(seq bindings)
                     ~(α-convert body env)))
      (meta form))))

(defmethod α-convert-seq 'qwerty/if [[_ condition then else] env]
  (doall
   `(qwerty/if ~(α-convert condition env)
      ~(α-convert then env)
      ~(α-convert else env))))

(defmethod α-convert-seq 'qwerty/definterface [form env]
  form)

(defmethod α-convert-seq 'qwerty/make [form env]
  form)

(defmethod α-convert-seq 'qwerty/labels [[_ & exps] env]
  `(qwerty/labels ~@(for [e exps]
                      (if (symbol? e)
                        e
                        (α-convert e env)))))

(defmethod α-convert-seq 'qwerty/test [[_ c l] env]
  `(qwerty/test ~(α-convert c env) ~l))

(defmethod α-convert-seq 'qwerty/goto [form env]
  form)

(defmethod α-convert-seq 'qwerty/map-entry [[_ map key] env]
  `(qwerty/map-entry ~(α-convert map env)
                     ~(α-convert key env)))

(defmethod α-convert-seq 'qwerty/map-update [[_ map key value] env]
  `(qwerty/map-update ~(α-convert map env)
                      ~(α-convert key env)
                      ~(α-convert value env)))

(defmethod α-convert-seq 'qwerty/go [[_ fun] env]
  `(qwerty/go ~(α-convert fun env)))

(defmethod α-convert-seq 'qwerty/go-method-call [[_ target method-name & args] env]
  `(qwerty/go-method-call ~(α-convert target env) ~method-name
                          ~@(for [a args]
                              (α-convert a env))))

(defmethod α-convert-seq 'qwerty/= [[_ a b] env]
  `(qwerty/= ~(α-convert a env)
             ~(α-convert b env)))

(defmethod α-convert-seq 'qwerty/go-> [[_ [value channel] body] env]
  `(qwerty/go-> (~(α-convert value env)
                 ~(α-convert channel env))
                ~(α-convert body env)))

(defmethod α-convert-seq 'qwerty/quote [exp env] exp)

(defmethod α-convert-seq 'qwerty/go<- [[_ [result channel] body] env]
  (if (coll? result)
    (let [[n ok] result
          nn (gensym 'n)
          ook (gensym 'ok)]
      (assert (symbol? n))
      (assert (symbol? ok))
      `(qwerty/go<- ((~nn ~ook) ~(α-convert channel env))
                    ~(α-convert body (assoc env
                                       n nn
                                       ok ook))))
    (let [nn (gensym 'n)]
      (assert (symbol? result))
      `(qwerty/go<- (~nn ~(α-convert channel env))
                    ~(α-convert body (assoc env
                                       result nn))))))


(defmethod α-convert-seq :default [exp env]
  (assert (not (and (symbol? (first exp))
                    (= "qwerty" (namespace (first exp)))))
          (first exp))
  (with-meta (doall (map #(α-convert % env) exp)) (meta exp)))
