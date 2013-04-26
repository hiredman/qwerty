(qwerty/package qwerty)
(qwerty/import math)

(qwerty/defgofun isub  (x y)
  ((interface interface) (interface))
  (qwerty/let* ((a (qwerty/cast int x))
                (b (qwerty/cast int y)))
               (qwerty/- a b)))

(qwerty/defgofun mult (x y)
  ((interface interface) (interface))
  (qwerty/let* ((a (qwerty/cast int x))
                (b (qwerty/cast int y)))
               (qwerty/* a b)))

(qwerty/defgofun pow (x y)
  ((interface interface) (interface))
  (qwerty/let* ((a (qwerty/cast float64 (qwerty/. float64 (qwerty/cast int x))))
                (b (qwerty/cast float64 (qwerty/. float64 (qwerty/cast int y))))
                (c (qwerty/. math.Pow a b))
                (c (qwerty/cast float64 c)))
               (qwerty/. int c)))

(qwerty/defgofun iadd (x y)
  ((interface interface) (interface))
  (qwerty/let* ((a (qwerty/cast int x))
                (b (qwerty/cast int y)))
               (qwerty/+ a b)))

(qwerty/defgofun sum (start end fun)
  ((interface interface interface) (interface))
  (qwerty/let* ((sum 0))
               (qwerty/do
                 (qwerty/labels
                  start
                  (qwerty/test (qwerty/= start (qwerty/. iadd end 1)) sum)
                  (qwerty/goto end)
                  sum
                  (qwerty/do
                    (qwerty/set! sum (qwerty/. iadd sum (fun start)))
                    (qwerty/set! start (qwerty/. iadd 1 start))
                    (qwerty/goto start))
                  end)
                 sum)))
