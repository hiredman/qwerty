(qwerty/package qwerty)
(qwerty/import math)

(qwerty/func isub ((qwerty/T x interface) (qwerty/T y interface)) ((qwerty/T _ interface))
  (qwerty/let* ((a (qwerty/cast int x))
                (b (qwerty/cast int y))
                (x (qwerty/- a b))
                (x (qwerty/cast int x)))
    (qwerty/return x)))

(qwerty/func mult ((qwerty/T x interface) (qwerty/T y interface)) ((qwerty/T _ interface))
  (qwerty/let* ((a (qwerty/cast int x))
                (b (qwerty/cast int y))
                (x (qwerty/* a b))
                (x (qwerty/cast int x)))
    (qwerty/return x)))

(qwerty/func pow ((qwerty/T x interface) (qwerty/T y interface)) ((qwerty/T _ interface))
  (qwerty/let* ((a (qwerty/cast float64 (qwerty/. float64 (qwerty/cast int x))))
                (b (qwerty/cast float64 (qwerty/. float64 (qwerty/cast int y))))
                (c (qwerty/. math.Pow a b))
                (c (qwerty/cast float64 c))
                (x (qwerty/. int c))
                (x (qwerty/cast int x)))
    (qwerty/return x)))

(qwerty/func iadd ((qwerty/T x interface) (qwerty/T y interface)) ((qwerty/T _ interface))
  (qwerty/let* ((a (qwerty/cast int x))
                (b (qwerty/cast int y))
                (x (qwerty/+ a b))
                (x (qwerty/cast int x)))
    (qwerty/return x)))

(qwerty/func i64add ((qwerty/T x interface) (qwerty/T y interface)) ((qwerty/T _ interface))
  (qwerty/let* ((a (qwerty/cast int64 x))
                (b (qwerty/cast int64 y))
                (x (qwerty/+ a b))
                (x (qwerty/cast int64 x)))
    (qwerty/return x)))

(qwerty/func sum
  ((qwerty/T start interface) (qwerty/T end interface) (qwerty/T fun interface))
  ((qwerty/T _ interface))
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
     (qwerty/return sum))))

(qwerty/def i64add
  (qwerty/fn* (a b)
    (qwerty/. i64add a b)))
