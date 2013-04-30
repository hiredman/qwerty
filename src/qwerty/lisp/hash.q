(qwerty/package qwerty)
(qwerty/import unicode/utf8)

;; hashcodes are int32

(qwerty/definterface Equaler
  (Equal (a2) (result1)))

(qwerty/definterface Hasher
  (HashCode () (result1)))

(qwerty/func hash_string ((qwerty/T str string)) ((qwerty/T _ int))
  (qwerty/let* ((n (qwerty/cast int (qwerty/. utf8.RuneCountInString str)))
                (v (qwerty/cast int (qwerty/. sum 0 (qwerty/. isub n 1)
                                              (qwerty/fn* (i)
                                                (qwerty/let* ((i (qwerty/cast int i))
                                                              (str (qwerty/cast string str))
                                                              (b (qwerty/nth* str i))
                                                              (b (qwerty/cast byte b))
                                                              (b (qwerty/. int b)))
                                                  (qwerty/. mult b (qwerty/. pow 31 (qwerty/. isub (qwerty/. isub n 1) i)))))))))
    (qwerty/return v)))


(qwerty/func Hash ((qwerty/T item interface)) ((qwerty/T _ int))
  (qwerty/let* ((r (qwerty/results (v ok) (qwerty/cast Hasher item)
                     (qwerty/if ok
                       (qwerty/let* ((v (qwerty/cast Hasher v)))
                         (qwerty/go-method-call v HashCode))
                       (qwerty/results (v ok) (qwerty/cast string item)
                         (qwerty/if ok
                           (qwerty/let* ((v (qwerty/cast string v)))
                             (qwerty/. hash_string v))
                           (qwerty/do
                            (qwerty/. panic "can't hash")
                            nil))))))
                (r (qwerty/cast int r)))
    (qwerty/return r)))

;; go interfaces are dumb
