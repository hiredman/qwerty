(qwerty/package main)
(qwerty/import "fmt")



;; (qwerty/defgofun anonX (x)
;;  ((string) string)
;;  x)

;; (qwerty/type AnonXfunc (string ->))

;; (qwerty/struct anonXenv
;;   _fun anonXfunc)

;; (qwerty/defgo foo anonXfunc (qwerty/new AnonXfunc anonX))

(qwerty/defgofun main ()
                 (())
                 (qwerty/let* ((x (qwerty/fn* (x) x))
                               (y (x "Hello World")))
                   (qwerty/. fmt.Println y)))

