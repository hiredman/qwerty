(qwerty/package qwerty)
(qwerty/import unicode/utf8)

(qwerty/definterface Equaler
  (Equal (a2) (result1)))

;; hashcodes are int32

(qwerty/definterface Hasher
  (HashCode (a2) (result1)))

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
                               (qwerty/let* ((a (qwerty/cast int32 x))
                                             (b (qwerty/cast int32 y)))
                                            (qwerty/- a b))))

(qwerty/defgofun Hash_string (str)
  ((string) (int32))
  (qwerty/let* ((n (qwerty/cast int (qwerty/. utf8.RuneCountInString str)))
                (v (qwerty/cast int (sum 0 (isub n 1)
                                         (qwerty/fn* (i)
                                                     (mult (rune_at str i)
                                                           (pow 31 (isub (isub n 1) i))))))))
               (qwerty/. int32 v)))



;; go interfaces are dumb


;; (qwerty/defgomethod Hash int (n) (r)
;;   ((interface) (interface))
;;   (qwerty/cast int32 n))

;; (qwerty/defgomethod Hash int32 (n) (r)
;;   ((interface) (interface))
;;   n)
