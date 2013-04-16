(qwerty/package main)
(qwerty/import "fmt")

(qwerty/struct Cons
               car interface
               cdr interface)

(qwerty/struct Var
               name string
               value interface
               macro bool)

(qwerty/godef make-var (qwerty/fn* (name value)
                                   (qwerty/let* ((v (qwerty/new Var))
                                                 (n (qwerty/cast string name)))
                                                (qwerty/do
                                                 (qwerty/set! (qwerty/.- v name) n)
                                                 (qwerty/set! (qwerty/.- v value) value)
                                                 v))))

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

(qwerty/defgofun stdin_int_ ()
                 (() int)
                 (qwerty/do
                  (qwerty/local x int)
                  (qwerty/. fmt.Scanf "%d" (qwerty/goderef x))
                  x))

(qwerty/godef stdin-int (qwerty/fn* () (qwerty/. stdin_int_)))

(qwerty/godef stdin-rune (qwerty/fn* () (qwerty/. stdin_rune_)))

(qwerty/defgofun main ()
                 (())
                 (qwerty/do
                  (qwerty/let* ((one "one"))
                               (qwerty/results (a b c) ((qwerty/fn* () (qwerty/values one "two" "three")))
                                               (qwerty/do
                                                (println a)
                                                (println b)
                                                (println c))))
                  (println "x")
                  ;; (println (iadd (stdin-int)
                  ;;                (stdin-int)))
                  (println (qwerty/nil? nil))
                  (println (iadd 1 2))
                  (println (cdr (cons "x" "y")))
                  (println "Hello World")))
