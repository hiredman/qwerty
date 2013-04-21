(qwerty/package qwerty)

(qwerty/struct ASymbol
               name string)

(qwerty/defgofun Symbol (name)
  ((interface) (interface))
  (qwerty/let* ((n (qwerty/cast string name))
                (s (qwerty/new ASymbol)))
               (qwerty/do
                 (qwerty/set! (qwerty/.- s name) n)
                 s)))


(qwerty/defgomethod String ASymbol (s) (r)
  (() (string))
  (qwerty/.- s name))
