(qwerty/package qwerty)
(qwerty/import fmt)

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

(qwerty/godef ListCount (qwerty/fn* (lst)
                                    (qwerty/if (qwerty/nil? lst)
                                      0
                                      (qwerty/. iadd 1 (Cdr lst)))))

(qwerty/godef deref (qwerty/fn* (v) (qwerty/go-method-call (qwerty/cast *AVar v) Deref)))

(qwerty/godef symbol (qwerty/fn* (n) (qwerty/. Symbol_ n)))
(qwerty/godef intern_var (qwerty/fn* (n v) (qwerty/. InternVar_ n v)))

(qwerty/let* ((mapV (qwerty/. Var_ (symbol "qwerty/map"))))
             (qwerty/do
              (qwerty/. fmt.Println "static init cons")
               (intern_var (symbol "qwerty/map")
                           (qwerty/fn* (f lst)
                                       (qwerty/if (qwerty/nil? lst)
                                         nil
                                         (Cons (f (Car lst))
                                               ((deref mapV) f (Cdr lst))))))
               (intern_var (symbol "qwerty/fold")
                           (qwerty/fn* (f init lst)
                                       (qwerty/do
                                         (qwerty/labels
                                          start
                                          (qwerty/test (qwerty/nil? lst) reduce)
                                          (qwerty/goto end)
                                          reduce
                                          (qwerty/do
                                            (qwerty/set! init (f init (Car lst)))
                                            (qwerty/set! lst (Cdr lst))
                                            (qwerty/goto start))
                                          end)
                                         init)))))
