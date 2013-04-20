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
                              (qwerty/let* ((foo (qwerty/cast *ACons c)))
                                           (qwerty/.- foo car))))

(qwerty/godef Cdr (qwerty/fn* (c)
                              (qwerty/let* ((foo (qwerty/cast *ACons c)))
                                           (qwerty/.- foo cdr))))

