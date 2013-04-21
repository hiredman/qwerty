(qwerty/package qwerty)

(qwerty/struct AVar
               name string
               value interface
               macro bool)

(qwerty/godef the_vars (qwerty/make "map[string]*AVar"))

(qwerty/defgomethod Deref *AVar (the_var) (r)
  ((interface) (interface))
  (qwerty/.- the_var value))

(qwerty/godef Var (qwerty/fn* (name)
                              (qwerty/let* ((n (qwerty/cast *ASymbol name))
                                            (n (qwerty/cast string (qwerty/.- n name)))
                                            (m (qwerty/cast "map[string]*AVar" the_vars)))
                                           (qwerty/results (value found) (qwerty/map-entry m n)
                                                           (qwerty/if found
                                                             value
                                                             (qwerty/let* ((v (qwerty/new AVar)))
                                                                          (qwerty/do
                                                                            (qwerty/set! (qwerty/.- v name) n)
                                                                            (qwerty/map-update m n v)
                                                                            v)))))))

;; (qwerty/defgofun Var (name)
;;   ((interface) (interface))
;;   )



(qwerty/defgofun InternVar (name value)
  ((interface interface) (interface))
  (qwerty/let* ((v (qwerty/cast *AVar (Var name))))
               (qwerty/do
                 (qwerty/set! (qwerty/.- v value) value)
                 v)))
