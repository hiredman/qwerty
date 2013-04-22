(qwerty/package qwerty)

(qwerty/struct AVar
               name string
               value interface
               macro bool)

(qwerty/godef the_vars (qwerty/make "map[string]*AVar"))

(qwerty/defgomethod Deref *AVar (the_var) (r)
  ((interface) (interface))
  (qwerty/.- the_var value))

(qwerty/defgofun Var_ (name)
  ((interface) (interface))
  (qwerty/let* ((n (qwerty/cast *ASymbol name))
                (n (qwerty/cast string (qwerty/.- n name))))
               (qwerty/results (value found) (qwerty/map-entry the_vars n)
                               (qwerty/if found
                                 value
                                 (qwerty/let* ((v (qwerty/new AVar)))
                                              (qwerty/do
                                                (qwerty/set! (qwerty/.- v name) n)
                                                (qwerty/map-update the_vars n v)
                                                v))))))

(qwerty/godef Var (qwerty/fn* (name) (qwerty/. Var_ name)))

(qwerty/defgofun InternVar_ (name value)
  ((interface interface) (interface))
  (qwerty/let* ((v (qwerty/cast *AVar (qwerty/. Var_ name))))
               (qwerty/do
                 (qwerty/set! (qwerty/.- v value) value)
                 v)))

(qwerty/godef InternVar (qwerty/fn* (name value) (qwerty/. InternVar_ name value)))

(qwerty/godef string_concat (qwerty/fn* (x y)
                                        (qwerty/let* ((a (qwerty/cast string x))
                                                      (b (qwerty/cast string y)))
                                                     (qwerty/+ a b))))

(qwerty/defgomethod String AVar (s) (r)
  (() (string))
  (qwerty/cast string
               (string_concat
                (string_concat
                 "(qwerty/var "
                 (qwerty/.- s name))
                ")")))
