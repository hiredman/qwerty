(qwerty/package qwerty)

(qwerty/struct AVar
               name string
               value interface
               macro bool)

(qwerty/godef the_vars (qwerty/make "map[string]*AVar"))

(qwerty/definterface Derefer
  (Deref () (bar)))

(qwerty/defgomethod Deref *AVar (the_var) (r)
  (() (interface))
  (qwerty/.- the_var value))

(qwerty/defgofun DerefF (obj)
  ((interface) (interface))
  (qwerty/let* ((o (qwerty/cast Derefer obj)))
    (qwerty/go-method-call o Deref)))

(qwerty/defgofun Var_ (name)
  ((interface) (interface))
  (qwerty/do
   (qwerty/let* ((n (qwerty/cast *ASymbol name))
                 (n (qwerty/cast string (qwerty/.- n name))))
     (qwerty/do
      (qwerty/results (value found) (qwerty/map-entry (qwerty/goref the_vars) n)
        (qwerty/do
         (qwerty/if found
           value
           (qwerty/let* ((v (qwerty/new AVar)))
             (qwerty/do
              (qwerty/set! (qwerty/.- v name) n)
              (qwerty/map-update (qwerty/goref the_vars) n v)
              v)))))))))

(qwerty/godef Var (qwerty/fn* (name) (qwerty/. Var_ name)))

(qwerty/defgofun InternVar_ (name value)
  ((interface interface) (interface))
  (qwerty/let* ((v (qwerty/cast *AVar (qwerty/. Var_ name))))
    (qwerty/do
     (qwerty/set! (qwerty/.- v value) value)
     v)))

(qwerty/godef InternVar (qwerty/fn* (name value) (qwerty/. InternVar_ name value)))

(qwerty/defgofun string_concat (x y)
  ((interface interface) (interface))
  (qwerty/let* ((a (qwerty/cast string x))
                (b (qwerty/cast string y)))
    (qwerty/+ a b)))

(qwerty/defgomethod String AVar (s) (r)
  (() (string))
  (qwerty/cast string
               (qwerty/. string_concat
                         (qwerty/. string_concat
                                   "(qwerty/var "
                                   (qwerty/.- s name))
                         ")")))
