(qwerty/package qwerty)

(qwerty/struct ASymbol
               name string)

(qwerty/godef Symbol
              (qwerty/fn* (name)
                          (qwerty/let* ((n (qwerty/cast string name))
                                        (s (qwerty/new ASymbol)))
                                       (qwerty/do
                                         (qwerty/set! (qwerty/.- s name) n)
                                         s))))

(qwerty/defgofun Symbol_ (name)
  ((interface) (interface))
  (qwerty/let* ((n (qwerty/cast string name))
                (s (qwerty/new ASymbol)))
               (qwerty/do
                 (qwerty/set! (qwerty/.- s name) n)
                 s)))

(qwerty/. InternVar_
          (qwerty/. Symbol_ "qwerty/symbol")
          (qwerty/fn* (name) (Symbol name)))

(qwerty/defgomethod String ASymbol (s) (r)
  (() (string))
  (qwerty/.- s name))
