(declare varize)
(defmulti varize-expression (fn [e env] (type e)))
(defmulti varize-expression-seq (fn [e env] (first e)))
(defmethod varize-expression clojure.lang.ISeq [s env]
  (varize-expression-seq s env))
(defmethod varize-expression Number [s env] [s env])
(defmethod varize-expression String [s env] [s env])
(defmethod varize-expression Character [s env] [s env])
(defmethod varize-expression clojure.lang.Symbol [s env]
  (cond
   (:var (meta s))
   [s env]
   (contains? env s)
   [s env]
   (contains? @global-env s)
   (let [v (with-meta (gensym 'v) {:var true})]
     [(if (= *package* 'qwerty)
        `(qwerty/. ~'DerefF ~v)
        `(qwerty/. ~'qwerty.DerefF ~v))
      (update-in env [:vars] assoc v s)])
   :else
   [s env]))
(defmethod varize-expression nil [s env] [s env])
(defmethod varize-expression-seq 'qwerty/struct [exp env] [exp env])
(defmethod varize-expression-seq 'qwerty/if [exp env]
  [exp env])
(defmethod varize-expression-seq 'qwerty/nil? [exp env] [exp env])
(defmethod varize-expression-seq 'qwerty/cast [exp env] [exp env])
(defmethod varize-expression-seq 'qwerty/.- [exp env] [exp env])
(defmethod varize-expression-seq 'qwerty/new [exp env] [exp env])
(defmethod varize-expression-seq 'qwerty/do [exp env] [exp env])
(defmethod varize-expression-seq 'qwerty/set! [exp env] [exp env])
(defmethod varize-expression-seq 'qwerty/. [exp env]
  (assert (every? (complement vector?) exp))
  [exp env])
(defmethod varize-expression-seq 'qwerty/defgofun [[_ name args types body :as exp] env]
  [exp (update-in env [:env] into args)])
(defmethod varize-expression-seq 'qwerty/godef [[_ n v :as exp] env]
  (swap! global-env conj n)
  [exp env])
(defmethod varize-expression-seq 'qwerty/let* [[_ bindings body] env]
  (let [{:keys [bindings env]}
        (reduce
         (fn [{:keys [env bindings]} [n value]]
           (let [[new-value new-env] (varize value (update-in env [:env] conj n))]
             {:env new-env
              :bindings (conj bindings (list n new-value))}))
         {:env env
          :bindings []} bindings)]
    [`(qwerty/let* ~(seq bindings)
                   ~body)
     env]))
(defmethod varize-expression-seq 'qwerty/fn* [[_ args body :as exp] env]
  [exp (update-in env [:env] into args)])
(defmethod varize-expression-seq :default [exp env]
  (assert (every? (complement vector?) exp))
  [exp env])

(defn varize [exp env]
  (expand exp env varize-expression))
