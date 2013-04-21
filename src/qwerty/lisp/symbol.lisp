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
