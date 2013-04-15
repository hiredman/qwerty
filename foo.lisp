(qwerty/package main)
(qwerty/import "fmt")

(qwerty/struct Cons
               car interface
               cdr interface)

(qwerty/godef println
              (qwerty/fn* (x)
                          (qwerty/do
                           (qwerty/. fmt.Println x)
                           nil)))

(qwerty/godef cons (qwerty/fn* (x y)
                               (qwerty/let* ((c (qwerty/new Cons)))
                                            (qwerty/do
                                             (qwerty/set! (qwerty/.- c car) x)
                                             (qwerty/set! (qwerty/.- c cdr) y)
                                             c))))

(qwerty/godef car (qwerty/fn* (c)
                              (qwerty/let* ((foo (qwerty/cast *Cons c)))
                                           (qwerty/.- foo car))))

(qwerty/godef cdr (qwerty/fn* (c)
                              (qwerty/let* ((foo (qwerty/cast *Cons c)))
                                           (qwerty/.- foo cdr))))

(qwerty/godef iadd (qwerty/fn* (x y)
                               (qwerty/let* ((a (qwerty/cast int x))
                                             (b (qwerty/cast int y)))
                                            (qwerty/+ a b))))

(qwerty/defgofun stdin_rune_ ()
                 (() rune)
                 (qwerty/do
                  (qwerty/local x rune)
                  (qwerty/. fmt.Scanf "%c" (qwerty/goderef x))
                  x))

(qwerty/godef stdin-rune (qwerty/fn* () (qwerty/. stdin_rune_)))

(qwerty/defgofun main ()
                 (())
                 (qwerty/do
                  (println "x")
                  (println (stdin-rune))
                  (println "y")
                  (println (stdin-rune))
                  (println (iadd 1 2))
                  (println (cdr (cons "x" "y")))
                  (println "Hello World")))
