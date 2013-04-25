(qwerty/package qwerty)

(qwerty/struct ACons
               car interface
               cdr interface
               hash interface)

(qwerty/defgofun CarF (c)
  ((interface) (interface))
  (qwerty/if (qwerty/nil? c)
    nil
    (qwerty/let* ((foo (qwerty/cast *ACons c)))
                 (qwerty/.- foo car))))

(qwerty/defgofun CdrF (c)
  ((interface) (interface))
  (qwerty/if (qwerty/nil? c)
    nil
    (qwerty/let* ((foo (qwerty/cast *ACons c)))
                 (qwerty/.- foo cdr))))

(qwerty/godef Cons (qwerty/fn* (x y)
                               (qwerty/let* ((c (qwerty/new ACons)))
                                            (qwerty/do
                                              (qwerty/set! (qwerty/.- c car) x)
                                              (qwerty/set! (qwerty/.- c cdr) y)
                                              c))))

(qwerty/godef Car (qwerty/fn* (c) (qwerty/. CarF c)))

(qwerty/godef Cdr (qwerty/fn* (c) (qwerty/. CdrF c)))

(qwerty/godef ListCount (qwerty/fn* (lst)
                                    (qwerty/if (qwerty/nil? lst)
                                      0
                                      (qwerty/. iadd 1 ((qwerty/goref ListCount) ((qwerty/goref Cdr) lst))))))

(qwerty/godef deref (qwerty/fn* (v) (qwerty/go-method-call (qwerty/cast *AVar v) Deref)))

(qwerty/godef symbol (qwerty/fn* (n) (qwerty/. Symbol_ n)))
(qwerty/godef intern_var (qwerty/fn* (n v) (qwerty/. InternVar_ n v)))


(qwerty/def lisp/map (qwerty/fn* (f lst)
                                 (qwerty/if (qwerty/nil? lst)
                                   nil
                                   ((qwerty/goref Cons)
                                    (f ((qwerty/goref Car) lst))
                                    (lisp/map f ((qwerty/goref Cdr) lst))))))

(qwerty/def lisp/reduce
  (qwerty/fn* (f init lst)
              (qwerty/do
                (qwerty/labels
                 start
                 (qwerty/test (qwerty/nil? lst) reduce)
                 (qwerty/goto end)
                 reduce
                 (qwerty/do
                   (qwerty/set! init (f init ((qwerty/goref Car) lst)))
                   (qwerty/set! lst ((qwerty/goref Cdr) lst))
                   (qwerty/goto start))
                 end)
                init)))


(qwerty/defgomethod HashCode ACons (s) (r)
  (() (interface))
  (qwerty/do
    (qwerty/. panic "hashing cons")
    0))
