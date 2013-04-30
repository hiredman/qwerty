(declare varize)
(defmulti varize-expression (fn [e up-env down-env] (type e)))
(defmulti varize-expression-seq (fn [e up-env down-env] (first e)))
(defmethod varize-expression clojure.lang.ISeq [s up-env down-env]
  (varize-expression-seq s up-env down-env))
(defmethod varize-expression Number [s up-env down-env] [s up-env down-env])
(defmethod varize-expression String [s up-env down-env] [s up-env down-env])
(defmethod varize-expression Character [s up-env down-env] [s up-env down-env])
(defmethod varize-expression clojure.lang.Symbol [s up-env down-env]
  (cond
   (:var (meta s))
   [s up-env down-env]
   (contains? (:env down-env) s)
   [s up-env down-env]
   (and (contains? @global-env s)
        (contains? (set (vals (:vars up-env))) s))
   [(if (= *package* 'qwerty)
      `(qwerty/. ~'DerefF ~@(for [[k v] (:vars up-env)
                                  :when (= v s)]
                              k))
      `(qwerty/. ~'qwerty.DerefF ~@(for [[k v] (:vars up-env)
                                         :when (= v s)]
                                     k)))
    up-env
    down-env]
   (contains? @global-env s)
   (let [v (with-meta (gensym (munge s)) {:var true})]
     [(if (= *package* 'qwerty)
        `(qwerty/. ~'DerefF ~v)
        `(qwerty/. ~'qwerty.DerefF ~v))
      (update-in up-env [:vars] assoc v s)
      down-env])
   :else
   (do
     (binding [*out* *err*]
       (prn up-env)
       (prn down-env))
     (throw (Exception. (str "unkown name " s))))))
(defmethod varize-expression nil [s up-env down-env] [s up-env down-env])
(defmethod varize-expression-seq 'qwerty/struct [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/if [exp up-env down-env]
  [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/nil? [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/comment [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/cast [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/.- [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/new [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/do [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/return [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/set! [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/goref [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/go-method-call [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/def [[_ n v :as exp] up-env down-env]
  (swap! global-env conj n)
  [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/test [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/goto [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/definterface [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/nth* [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/- [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/+ [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/* [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/= [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/make [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/quote [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/map-update [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/map-entry [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/values [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/go [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/go-> [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/local [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/defer [exp up-env down-env] [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/go<- [[_ [result channel] body :as exp] up-env down-env]
  [exp up-env (update-in down-env [:env] conj result)])
(defmethod varize-expression-seq 'qwerty/results [[_ values app body] up-env down-env]
  (let [[new-app new-up-env _] (varize app up-env down-env)]
    [`(qwerty/results ~values ~new-app
                      ~body)
     new-up-env
     (update-in down-env [:env] into values)]))
(defmethod varize-expression-seq 'qwerty/. [exp up-env down-env]
  (assert (every? (complement vector?) exp))
  [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/godef [[_ n v :as exp] up-env down-env]
  [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/let* [[_ bindings body] up-env down-env]
  (let [{:keys [bindings up-env down-env]}
        (reduce
         (fn [{:keys [up-env down-env bindings]} [n value]]
           (let [[new-value new-up-env new-down-env] (varize value up-env down-env)]
             {:up-env new-up-env
              :down-env (update-in down-env [:env] (comp set conj) n)
              :bindings (conj bindings (list n new-value))}))
         {:up-env up-env
          :down-env down-env
          :bindings []} bindings)]
    [`(qwerty/let* ~(seq bindings)
                   ~body)
     up-env
     down-env]))
(defmethod varize-expression-seq 'qwerty/fn* [[_ args body :as exp] up-env down-env]
  [exp up-env (update-in down-env [:env] (comp set into) args)])
(defmethod varize-expression-seq 'qwerty/labels [[_ & body :as exp] up-env down-env]
  (let [{:keys [up-env down-env body]}
        (reduce
         (fn [{:keys [up-env down-env body]} form]
           (if (symbol? form)
             {:up-env up-env
              :down-env down-env
              :body (conj body form)}
             (let [[new-form new-up-env new-down-env] (varize form up-env down-env)]
               {:up-env new-up-env
                :down-env down-env
                :body (conj body new-form)})))
         {:up-env up-env
          :down-env down-env
          :body []}
         body)]
    [`(qwerty/labels ~@body)
     up-env
     down-env]))
(defmethod varize-expression-seq :default [exp up-env down-env]
  (assert (every? (complement vector?) exp))
  (assert (not (and (symbol? (first exp))
                    (= "qwerty" (namespace (first exp)))))
          (pr-str exp))
  [exp up-env down-env])
(defmethod varize-expression-seq 'qwerty/func [[_ name-or-target-type :as exp] up-env down-env]
  (if (symbol? name-or-target-type)
    (let [[_ name args returns body] exp]
      [exp up-env (update-in down-env [:env] (comp set into) (map second args))])
    (let [[_ target name args returns body] exp]
      [exp up-env (update-in down-env [:env] (comp set into) (cons (second target) (map second args)))])))

(defn varize [exp up-env down-env]
  (expand exp up-env down-env varize-expression))
