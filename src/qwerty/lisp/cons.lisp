(qwerty/package qwerty)

(qwerty/struct ACons
               car interface
               cdr interface)

(qwerty/godef Cons (qwerty/fn* (x y)
                               (qwerty/let* ((c (qwerty/new ACons)))
                                            (qwerty/do
                                              (qwerty/set! (qwerty/.- c car) x)
                                              (qwerty/set! (qwerty/.- c cdr) y)
                                              c))))

(qwerty/godef Car (qwerty/fn* (c)
                              (qwerty/if (qwerty/nil? c)
                                nil
                                (qwerty/let* ((foo (qwerty/cast *ACons c)))
                                             (qwerty/.- foo car)))))

(qwerty/godef Cdr (qwerty/fn* (c)
                              (qwerty/if (qwerty/nil? c)
                                nil
                                (qwerty/let* ((foo (qwerty/cast *ACons c)))
                                             (qwerty/.- foo cdr)))))

(qwerty/godef iadd (qwerty/fn* (x y)
                               (qwerty/let* ((a (qwerty/cast int x))
                                             (b (qwerty/cast int y)))
                                            (qwerty/+ a b))))

(qwerty/godef ListCount (qwerty/fn* (lst)
                                    (qwerty/if (qwerty/nil? lst)
                                      0
                                      (iadd 1 (Cdr lst)))))

(qwerty/godef mapV (Var (Symbol "qwerty/map")))

(qwerty/godef _
              (qwerty/. InternVar (Symbol "qwerty/map")
                        (qwerty/fn* (f lst)
                                    (qwerty/if (qwerty/nil? lst)
                                      nil
                                      (Cons (f (Car lst))
                                            ((qwerty/go-method-call (qwerty/cast *AVar mapV) Deref) f (Cdr lst)))))))





