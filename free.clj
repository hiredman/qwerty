(declare free-variables)

(defmulti free (fn [exp env] (type exp)))
(defmulti free-seq (fn [exp env] (first exp)))
(defmethod free clojure.lang.ISeq [exp env]
  (free-seq exp env))
(defmethod free-seq 'qwerty/do [exp env] [exp env])
(defmethod free-seq 'qwerty/. [exp env] [exp env])
(defmethod free clojure.lang.Symbol [exp env]
  (if (contains? (:bound env) exp)
    [exp env]
    [exp (update-in env [:free] conj exp)]))
(defmethod free nil [exp env] [exp env])
(defmethod free-seq 'qwerty/let* [[_ bindings body :as exp] env]
  [exp
   (reduce
    (fn [env [n value]]
      (let [[_ new-env] (expand value env free)]
        (update-in new-env [:bound] conj n)))
    env bindings)])
(defmethod free-seq 'qwerty/cast [exp env] [exp env])
(defmethod free-seq 'qwerty/results [[_ values app body :as exp] env]
  [exp (update-in env [:bound] into values)])
(defmethod free-seq :default [exp env] [exp env]
  (assert (not (and (symbol? (first exp))
                    (= "qwerty" (namespace (first exp)))))
          (pr-str exp))
  [exp env])
(defmethod free-seq 'qwerty/if [exp env] [exp env])
(defmethod free-seq 'qwerty/nil? [exp env] [exp env])
(defmethod free java.lang.String [exp env] [exp env])
(defmethod free-seq 'qwerty/new [exp env] [exp env])
(defmethod free-seq 'qwerty/set! [exp env] [exp env])
(defmethod free-seq 'qwerty/.- [exp env] [exp env])
(defmethod free-seq 'qwerty/+ [exp env] [exp env])
(defmethod free-seq 'qwerty/values [exp env] [exp env])
(defmethod free java.lang.Number [exp env] [exp env])

(defn free-variables [exp]
  (:free (second (expand exp {:free #{}
                              :bound #{}}
                         free))))