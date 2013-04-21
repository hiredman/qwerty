(qwerty/package qwerty)

(qwerty/struct AVar
               name string
               value interface
               macro bool)

(qwerty/godef the_vars (qwerty/make "map[string]*AVar"))

(qwerty/defgomethod DerefMethod *AVar (the_var) (r)
  (qwerty/.- the_var value))

(qwerty/godef Deref (qwerty/fn* (v)
                                (qwerty/let* ((v (qwerty/cast *AVar v)))
                                             (qwerty/go-method-call v DerefMethod))))

(qwerty/defgofun Var (name)
  ((interface) (interface))
  (qwerty/let* ((n (qwerty/cast string name))
                (m (qwerty/cast "map[string]*AVar" the_vars)))
               (qwerty/results (value found) (qwerty/map-entry m n)
                               (qwerty/if found
                                 value
                                 (qwerty/let* ((v (qwerty/new AVar)))
                                              (qwerty/do
                                                (qwerty/set! (qwerty/.- v name) n)
                                                (qwerty/map-update m n v)
                                                v))))))



(qwerty/defgofun InternVar (name value)
  ((interface interface) (interface))
  (qwerty/let* ((v (qwerty/cast *AVar (qwerty/. Var name))))
               (qwerty/do
                 (qwerty/set! (qwerty/.- v value) value)
                 v)))
