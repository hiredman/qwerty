(qwerty/package qwerty)

(qwerty/struct ACons
               car interface
               cdr interface
               hash interface)

(qwerty/defgofun CarF (c)
  ((interface) (interface))
  (qwerty/if (qwerty/nil? c)
    nil
    (qwerty/results (foo ok) (qwerty/cast *ACons c)
                    (qwerty/if ok
                      (qwerty/let* ((foo (qwerty/cast *ACons foo)))
                                   (qwerty/.- foo car))
                      (qwerty/let* ((foo (qwerty/cast ACons c)))
                                   (qwerty/.- foo car))))))

(qwerty/defgofun CdrF (c)
  ((interface) (interface))
  (qwerty/if (qwerty/nil? c)
    nil
    (qwerty/results (foo ok) (qwerty/cast *ACons c)
                    (qwerty/if ok
                      (qwerty/let* ((foo (qwerty/cast *ACons foo)))
                                   (qwerty/.- foo cdr))
                      (qwerty/let* ((foo (qwerty/cast ACons c)))
                                   (qwerty/.- foo cdr))))))

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

(qwerty/def cons
  (qwerty/fn* (a b) ((qwerty/goref Cons) a b)))

(qwerty/godef deref (qwerty/fn* (v) (qwerty/go-method-call (qwerty/cast *AVar v) Deref)))

(qwerty/godef symbol (qwerty/fn* (n) (qwerty/. Symbol_ n)))
(qwerty/godef intern_var (qwerty/fn* (n v) (qwerty/. InternVar_ n v)))


(qwerty/def lisp/map (qwerty/fn* (f lst)
                                 (qwerty/if (qwerty/nil? lst)
                                   nil
                                   ((qwerty/goref Cons)
                                    (f ((qwerty/goref Car) lst))
                                    (lisp/map f ((qwerty/goref Cdr) lst))))))

(qwerty/def lisp/fold
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


(qwerty/def lisp/reverse
  (qwerty/fn* (lst) (lisp/fold cons nil lst)))


(qwerty/defgomethod HashCode ACons (s) (r)
  (() (interface))
  (qwerty/do
    (qwerty/. panic "hashing cons")
    0))

(qwerty/def nop (qwerty/fn* (x) x))

;; this is so gross
(qwerty/defgomethod String ACons (s) (r)
  (() (string))
  (qwerty/let* ((r "(")
                (lst s))
               (qwerty/do
                 (qwerty/labels
                  (qwerty/goto item)
                  space
                  (qwerty/set! r ((qwerty/goref string_append) r " "))
                  item
                  (qwerty/let* ((first ((qwerty/goref Car) lst))
                                (rest ((qwerty/goref Cdr) lst)))
                               (nop
                                (qwerty/if (qwerty/= rest nil)
                                  (qwerty/do
                                    (qwerty/set! r ((qwerty/goref string_append) r ((qwerty/goref PrStr) first)))
                                    (qwerty/goto end)
                                    nil)
                                  (qwerty/do
                                    (qwerty/set! r ((qwerty/goref string_append) r ((qwerty/goref PrStr) first)))
                                    (qwerty/set! lst rest)
                                    (qwerty/goto space)
                                    nil))))
                  end)
                 (qwerty/set! r ((qwerty/goref string_append) r ")"))
                 (qwerty/let* ((r (qwerty/cast string r)))
                              r))))
