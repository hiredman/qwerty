(qwerty/package qwerty)

(qwerty/struct ACons
               car interface
               cdr interface)

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

(qwerty/godef iadd (qwerty/fn* (x y)
                               (qwerty/let* ((a (qwerty/cast int x))
                                             (b (qwerty/cast int y)))
                                            (qwerty/+ a b))))

(qwerty/godef ListCount (qwerty/fn* (lst)
                                    (qwerty/if (qwerty/nil? lst)
                                      0
                                      (iadd 1 (Cdr lst)))))

(qwerty/godef deref (qwerty/fn* (v) (qwerty/go-method-call (qwerty/cast *AVar v) Deref)))

;; example of what qwerty/def should expand in to
(qwerty/godef _
              (InternVar (Symbol "qwerty/map")
                         (qwerty/let* ((mapV (Var (Symbol "qwerty/map"))))
                                      (qwerty/fn* (f lst)
                                                  (qwerty/if (qwerty/nil? lst)
                                                    nil
                                                    (Cons (f (Car lst))
                                                          ((deref mapV) f (Cdr lst))))))))
