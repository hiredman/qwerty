(qwerty/package qwerty)
(qwerty/import unicode/utf8)
(qwerty/import math)

;; hashcodes are int32

(qwerty/definterface Hasher
  (Equal (a2) (result1))
  (HashCode () (result1)))

(qwerty/godef sum
              (qwerty/fn* (start end fun)
                          (qwerty/let* ((sum 0))
                                       (qwerty/do
                                         (qwerty/labels
                                          start
                                          (qwerty/test (qwerty/= start (iadd end 1)) sum)
                                          (qwerty/goto end)
                                          sum
                                          (qwerty/do
                                            (qwerty/set! sum (iadd sum (fun start)))
                                            (qwerty/set! start (iadd 1 start))
                                            (qwerty/goto start))
                                          end)
                                         sum))))

(qwerty/godef isub (qwerty/fn* (x y)
                               (qwerty/let* ((a (qwerty/cast int x))
                                             (b (qwerty/cast int y)))
                                            (qwerty/- a b))))

(qwerty/godef mult (qwerty/fn* (x y)
                               (qwerty/let* ((a (qwerty/cast int x))
                                             (b (qwerty/cast int y)))
                                            (qwerty/* a b))))

(qwerty/godef pow (qwerty/fn* (x y)
                              (qwerty/let* ((a (qwerty/cast float64 (qwerty/. float64 (qwerty/cast int x))))
                                            (b (qwerty/cast float64 (qwerty/. float64 (qwerty/cast int y))))
                                            (c (qwerty/. math.Pow a b))
                                            (c (qwerty/cast float64 c)))
                                           (qwerty/. int c))))

(qwerty/defgofun Hash_string (str)
  ((string) (int))
  (qwerty/let* ((n (qwerty/cast int (qwerty/. utf8.RuneCountInString str)))
                (v (qwerty/cast int (sum 0 (isub n 1)
                                         (qwerty/fn* (i)
                                                     (qwerty/let* ((i (qwerty/cast int i))
                                                                   (str (qwerty/cast string str))
                                                                   (b (qwerty/nth* str i))
                                                                   (b (qwerty/cast byte b))
                                                                   (b (qwerty/. int b)))
                                                                  (mult b
                                                                        (pow 31 (isub (isub n 1) i)))))))))
               v))


(qwerty/defgofun Hash (item)
  ((interface) (int))
  (qwerty/let* ((r (qwerty/results (v ok) (qwerty/cast Hasher item)
                                   (qwerty/if ok
                                     (qwerty/let* ((v (qwerty/cast Hasher v)))
                                                  (qwerty/go-method-call v HashCode))
                                     (qwerty/results (v ok) (qwerty/cast string item)
                                                     (qwerty/if ok
                                                       (qwerty/let* ((v (qwerty/cast string v)))
                                                                    (qwerty/. Hash_string v))
                                                       (qwerty/do
                                                         (qwerty/. panic "can't hash")
                                                         nil)))))))
               (qwerty/cast int r)))






;; go interfaces are dumb


;; (qwerty/defgomethod Hash int (n) (r)
;;   ((interface) (interface))
;;   (qwerty/cast int32 n))

;; (qwerty/defgomethod Hash int32 (n) (r)
;;   ((interface) (interface))
;;   n)
