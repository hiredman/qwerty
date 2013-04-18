(qwerty/package main)
(qwerty/import "fmt")
;; (qwerty/import "types")

(qwerty/struct Cons
               car interface
               cdr interface)

(qwerty/struct Var
               name string
               value interface
               macro bool)

(qwerty/definterface IFn

                     (invoke0_1 () (result1))
                     (invoke0_2 () (result1 result2))
                     (invoke0_3 () (result1 result2 result3))
                     (invoke0_4 () (result1 result2 result3 result4))

                     (invoke1_1 (a1) (result1))
                     (invoke1_2 (a1) (result1 result2))
                     (invoke1_3 (a1) (result1 result2 result3))
                     (invoke1_4 (a1) (result1 result2 result3 result4))

                     (invoke2_1 (a1 a2) (result1))
                     (invoke2_2 (a1 a2) (result1 result2))
                     (invoke2_3 (a1 a2) (result1 result2 result3))
                     (invoke2_4 (a1 a2) (result1 result2 result3 result4))
                     )

(qwerty/godef make-var (qwerty/fn* (name value)
                                   (qwerty/let* ((v (qwerty/new Var))
                                                 (n (qwerty/cast string name)))
                                                (qwerty/do
                                                 (qwerty/set! (qwerty/.- v name) n)
                                                 (qwerty/set! (qwerty/.- v value) value)
                                                 (qwerty/set! (qwerty/.- v macro) false)
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

(qwerty/defgofun test1 ()
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

(qwerty/defgofun main ()
                 (())
                 (qwerty/do
                  (qwerty/. test1)
                  #_(qwerty/if true
                               (println "TRUE")
                               (println "FALSE"))))


;; (qwerty/godef foo
;;               (qwerty/cast IFn
;;                            (qwerty/fn* (x)
;;                                        (qwerty/do
;;                                         (qwerty/. fmt.Println x)
;;                                         nil))))

;; (qwerty/defgofun main ()
;;                  (())
;;                  (foo "hello world"))


;; (qwerty/defgofun main ()
;;                  (())
;;                  ((qwerty/fn* ()
;;                               (qwerty/do
;;                                (qwerty/. fmt.Println "Hello World")
;;                                nil))))

;; (qwerty/defgofun main ()
;;                  (())
;;                  (qwerty/let* ((v (qwerty/new Var)))
;;                               (qwerty/. fmt.Println (qwerty/go-method-call v invoke0_1))))


;; (qwerty/select
;;  (qwerty/go<- ((i3,ok) ch)
;;               body)

;;  (qwerty/go-> (value ch)
;;               body))
