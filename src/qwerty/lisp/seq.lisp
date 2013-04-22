(qwerty/package qwerty)

(qwerty/definterface ISeq
  (First () (r))
  (Rest () (r)))

;; (qwerty/godef symbol (qwerty/fn* (n) (qwerty/. Symbol_ n)))
;; (qwerty/godef intern_var (qwerty/fn* (n v) (qwerty/. InternVar_ n v)))


(intern_var (qwerty/quote qwerty/first)
            (qwerty/fn* (s)
                        (qwerty/if (qwerty/nil? s)
                          nil
                          (qwerty/let* ((s (qwerty/cast ISeq s)))
                                       (qwerty/go-method-call s First)))))

(intern_var (qwerty/quote qwerty/rest)
            (qwerty/fn* (s)
                        (qwerty/if (qwerty/nil? s)
                          nil
                          (qwerty/let* ((s (qwerty/cast ISeq s)))
                                       (qwerty/go-method-call s Rest)))))

