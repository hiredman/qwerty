(qwerty/package qwerty)

(qwerty/struct AVar
  (qwerty/T name string)
  (qwerty/T value interface)
  (qwerty/T macro bool))

(qwerty/godef the_vars (qwerty/make "map[string]*AVar"))

(qwerty/definterface Derefer
  (Deref () (bar)))

(qwerty/func (qwerty/T the_var AVar) Deref () ((qwerty/T _ interface))
  (qwerty/return (qwerty/.- the_var value)))

(qwerty/func DerefF ((qwerty/T obj interface)) ((qwerty/T _ interface))
  (qwerty/return
   (qwerty/let* ((o (qwerty/cast Derefer obj)))
     (qwerty/go-method-call o Deref))))

(qwerty/func Var_ ((qwerty/T name interface)) ((qwerty/T _ interface))
  (qwerty/return
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

(qwerty/func InternVar_
  ((qwerty/T name interface) (qwerty/T value interface)) ((qwerty/T _ interface))
  (qwerty/let* ((v (qwerty/cast *AVar (qwerty/. Var_ name))))
    (qwerty/do
     (qwerty/set! (qwerty/.- v value) value)
     (qwerty/return v))))

(qwerty/godef InternVar (qwerty/fn* (name value) (qwerty/. InternVar_ name value)))

(qwerty/func string_concat
  ((qwerty/T x interface) (qwerty/T y interface)) ((qwerty/T _ interface))
  (qwerty/let* ((a (qwerty/cast string x))
                (b (qwerty/cast string y)))
    (qwerty/return (qwerty/+ a b))))

(qwerty/func (qwerty/T s AVar) String () ((qwerty/T _ string))
  (qwerty/let* ((s (qwerty/cast string
                                (qwerty/. string_concat
                                          (qwerty/. string_concat
                                                    "(qwerty/var "
                                                    (qwerty/.- s name))
                                          ")"))))
    (qwerty/return s)))
