(qwerty/package main)
(qwerty/import "fmt")



;; (qwerty/defgofun anonX (x)
;;  ((string) string)
;;  x)

;; (qwerty/type AnonXfunc (string ->))

;; (qwerty/struct anonXenv
;;   _fun anonXfunc)

;; (qwerty/defgo foo anonXfunc (qwerty/new AnonXfunc anonX))

;; (qwerty/defgofun main ()
;;   (())
;;   (qwerty/let* ((y "Hello World")
;;                 (z (qwerty/fn* () y))
;;                 (s (z)))
;;     (qwerty/. fmt.Println s)))

(qwerty/struct Cons
               car interface
               cdr interface)

(qwerty/defgofun main ()
  (())
  (qwerty/let*
   ((println (qwerty/fn* (x)
                         (qwerty/do
                          (qwerty/. fmt.Println x)
                          nil)))
    (cons (qwerty/fn* (x y)
                      (qwerty/let* ((c (qwerty/new Cons)))
                                   (qwerty/do
                                    (qwerty/set! (qwerty/.- c car) x)
                                    (qwerty/set! (qwerty/.- c cdr) y)
                                    c))))
    (car (qwerty/fn* (c)
                     (qwerty/let* ((foo (qwerty/cast *Cons c)))
                                  (qwerty/.- foo car))))
    (cdr (qwerty/fn* (c)
                     (qwerty/let* ((foo (qwerty/cast *Cons c)))
                                  (qwerty/.- foo cdr))))
    (iadd (qwerty/fn* (x y)
                      (qwerty/+ x y))))
   (qwerty/do
    (println (iadd 1 2))
    (println car)
    (println (cdr (cons "x" "y")))
    (println "Hello World"))))

