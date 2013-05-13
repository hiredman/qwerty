(defmulti hoist* (fn [e _ _] (type e)))
(defmulti hoist*-seq (fn [e _ _] (first e)))
(defmethod hoist* clojure.lang.Symbol [s a b] [s a b])
(defmethod hoist* Number [s a b] [s a b])
(defmethod hoist* nil [s a b] [s a b])
(defmethod hoist* String [s a b] [s a b])
(defmethod hoist* Boolean [s a b] [s a b])
(defmethod hoist* Character [s a b] [s a b])
(defmethod hoist* clojure.lang.ISeq [s a b]
  #(hoist*-seq s a b))
(defmethod hoist*-seq 'qwerty/struct [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/func [[_ name-or-target :as form] up-env down-env]
  (if (symbol? name-or-target)
    (let [[_ name args returns body] form]
      (if (and (seq? body)
               (= 'qwerty/do (first body)))
        (let [[_ & body] body
              hoisted (filter #(and (seq? %)
                                    (= 'qwerty/hoist (first %)))
                              body)
              not-hoisted (remove #(and (seq? %)
                                        (= 'qwerty/hoist (first %)))
                                  body)]
          [(if (seq hoisted)
             `(qwerty/do
                ~@(doall hoisted)
                (qwerty/func ~name ~args ~returns
                             (qwerty/do
                               ~@(doall not-hoisted))))
             `(qwerty/func ~name ~args ~returns
                           (qwerty/do
                             ~@(doall not-hoisted))))
           up-env down-env])
        [form up-env down-env]))
    (let [[_ target name args returns body] form]
      (if (and (seq? body)
               (= 'qwerty/do (first body)))
        (let [[_ & body] body
              hoisted (filter #(and (seq? %)
                                    (= 'qwerty/hoist (first %)))
                              body)
              not-hoisted (remove #(and (seq? %)
                                        (= 'qwerty/hoist (first %)))
                                  body)]
          [(if (seq hoisted)
             `(qwerty/do
                ~@(doall hoisted)
                (qwerty/func ~target ~name ~args ~returns
                             (qwerty/do
                               ~@(doall not-hoisted))))
             `(qwerty/func ~target ~name ~args ~returns
                           (qwerty/do
                             ~@(doall not-hoisted))))
           up-env down-env])
        [form up-env down-env]))))
(defmethod hoist*-seq 'qwerty/comment [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/nth* [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/definterface [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/local [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/set! [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/new [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/return [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/.- [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/nil? [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/test [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/goto [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/. [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/- [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/* [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/+ [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/godef [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/make [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/map-update [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/map-entry [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/= [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/go<- [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/go-> [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/go [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/defer [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/hoist [[_ v :as form] up-env down-env]
  (cond
   (and (seq? v) (= 'qwerty/do (first v)))
   [`(qwerty/do ~@(for [e (rest v)] `(qwerty/hoist ~e))) up-env down-env]
   (and (seq? v) (= 'qwerty/hoist (first v)))
   [v up-env down-env]
   :else [form up-env down-env]))
(defmethod hoist*-seq 'qwerty/values [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/cast [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/go-method-call [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/goref [exp up-env down-env] [exp up-env down-env])
(defmethod hoist*-seq 'qwerty/results [[_ results app body] up-env down-env]
  [`(qwerty/results ~results ~app ~body) up-env down-env])
(defmethod hoist*-seq 'qwerty/do [[_ & forms] up-env down-env]
  [`(qwerty/do ~@(for [form forms
                       form (if (and (seq? form)
                                     (= 'qwerty/do (first form)))
                              (let [{:keys [e h]} (reduce
                                                   (fn [{:keys [e h]} form]
                                                     (if (and (seq? form)
                                                              (= 'qwerty/hoist (first form)))
                                                       {:e e :h (conj h form)}
                                                       {:e (conj e form) :h h}))
                                                   {:e [] :h []}
                                                   (rest form))]
                                (concat h
                                        e))
                              [form])]
                   form))
   up-env down-env])
(declare hoist)
(defmethod hoist*-seq 'qwerty/labels [[_ & forms] up-env down-env]
  (let [x (for [form forms]
            (if (and (seq? form)
                     (= 'qwerty/do (first form)))
              (let [[_ & forms] (hoist form)
                    hoisted (for [form forms
                                  :when (and (seq? form)
                                             (= 'qwerty/hoist (first form)))]
                              form)
                    not-hoisted (for [form forms
                                      :when (not (and (seq? form)
                                                      (= 'qwerty/hoist (first form))))]
                                  form)]
                {:e `(qwerty/do ~@not-hoisted)
                 :h hoisted})
              {:e form}))]
    (if (seq (mapcat :h x))
      [`(qwerty/do
          ~@(mapcat :h x)
          (qwerty/labels ~@(map :e x))) up-env down-env]
      [`(qwerty/labels ~@(map :e x)) up-env down-env])))

(defn hoist [form]
  (first (expand form {} {} hoist*)))
