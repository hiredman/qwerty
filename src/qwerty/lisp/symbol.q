(qwerty/package qwerty)

(qwerty/struct ASymbol
               name string
               hash int)

(qwerty/defgofun Symbol_ (name)
  ((interface) (interface))
  (qwerty/let* ((n (qwerty/cast string name))
                (s (qwerty/new ASymbol))
                (h (qwerty/cast int (qwerty/. Hash n))))
               (qwerty/do
                 (qwerty/set! (qwerty/.- s name) n)
                 (qwerty/set! (qwerty/.- s hash) h)
                 s)))

(qwerty/godef Symbol (qwerty/fn* (name) (qwerty/. Symbol_ name)))

(qwerty/. InternVar_
          (qwerty/. Symbol_ "qwerty/symbol")
          (qwerty/fn* (name) ((qwerty/goref Symbol) name)))

(qwerty/defgomethod String ASymbol (s) (r)
  (() (string))
  (qwerty/.- s name))


(qwerty/defgomethod HashCode ASymbol (s) (r)
  (() (interface))
  (qwerty/.- s hash))
