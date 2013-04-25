(qwerty/package qwerty)

(qwerty/definterface ISeq
  (First () (r))
  (Rest () (r)))

(qwerty/def lisp/first
  (qwerty/fn* (s)
              (qwerty/if (qwerty/nil? s)
                nil
                (qwerty/let* ((s (qwerty/cast ISeq s)))

                             (qwerty/go-method-call s First)))))

(qwerty/def lisp/rest
  (qwerty/fn* (s)
              (qwerty/if (qwerty/nil? s)
                nil
                (qwerty/let* ((s (qwerty/cast ISeq s)))
                             (qwerty/go-method-call s Rest)))))
