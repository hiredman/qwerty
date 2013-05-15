(declare free-variables)

(defmulti free (fn [exp up-env down-env] (type exp)))
(defmulti free-seq (fn [exp up-env down-env] (first exp)))
(defmethod free clojure.lang.ISeq [exp up-env down-env]
  (free-seq exp up-env down-env))
(defmethod free-seq 'qwerty/do [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/. [exp up-env down-env] [exp up-env down-env])
(defmethod free clojure.lang.Symbol [exp up-env down-env]
  (if (contains? down-env exp)
    [exp up-env down-env]
    [exp (conj up-env exp) down-env]))
(defmethod free nil [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/let* [[_ bindings body :as exp] up-env down-env]
  (let [[new-up-env new-down-env]
        (reduce
         (fn [[up-env down-env] [n value]]
           (let [[_ new-up-env new-down-env] (expand value up-env down-env free)]
             [new-up-env (conj down-env n)]))
         [up-env down-env]
         bindings)]
    [exp new-up-env new-down-env]))
(defmethod free-seq 'qwerty/cast [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/results [[_ values app body :as exp] up-env down-env]
  [exp up-env (into down-env values)])
(defmethod free-seq :default [exp up-env down-env] [exp up-env down-env]
  (assert (not (and (symbol? (first exp))
                    (= "qwerty" (namespace (first exp)))))
          (pr-str exp))
  [exp up-env down-env])
(defmethod free-seq 'qwerty/if [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/make [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/defer [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/nil? [exp up-env down-env] [exp up-env down-env])
(defmethod free java.lang.String [exp up-env down-env] [exp up-env down-env])
(defmethod free java.lang.Boolean [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/new [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/set! [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/.- [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/+ [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/values [exp up-env down-env] [exp up-env down-env])
(defmethod free java.lang.Number [exp up-env down-env] [exp up-env down-env])
(defmethod free java.lang.Character [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/go [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/go<- [[_ [result channel] body :as exp] up-env down-env]
  [exp up-env (conj down-env result)])
(defmethod free-seq 'qwerty/= [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/go-> [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/labels [[_ & body] up-env down-env]
  (let [[new-up-env new-down-env]
        (reduce
         (fn [[up-env down-env] exp]
           (if (symbol? exp)
             [up-env down-env]
             (let [[_ new-up-env new-down-env] (expand exp up-env down-env free)]
               [new-up-env new-down-env])))
         [up-env down-env] body)]
    [`(qwerty/labels ~@body)
     new-up-env
     new-down-env]))
(defmethod free-seq 'qwerty/test [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/goto [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/go-method-call [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/fn* [[_ args body :as exp] up-env down-env]
  [exp up-env (into down-env args)])
(defmethod free-seq 'qwerty/map-update [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/map-entry [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/quote [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/- [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/* [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/nth* [exp up-env down-env] [exp up-env down-env])
(defmethod free-seq 'qwerty/goref [exp up-env down-env] [exp up-env down-env])

(defn free-variables [exp]
  (second (expand exp #{} #{} free)))
