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
  (qwerty/let* ((y "Hello World")
                (z (qwerty/fn* () y))
                (s (z)))
    (qwerty/. fmt.Println s)))

