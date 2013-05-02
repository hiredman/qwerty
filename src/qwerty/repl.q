(qwerty/package main)
(qwerty/import fmt)
(qwerty/import reflect)

(qwerty/godef println
  (qwerty/fn* (x)
    (qwerty/do
     (qwerty/. fmt.Println x)
     nil)))

(qwerty/def println
  (qwerty/fn* (x) ((qwerty/goref println) x)))

(qwerty/def print-prompt
  (qwerty/fn* ()
    (qwerty/do
     (qwerty/. fmt.Print "* ")
     nil)))

(qwerty/def print-notice
  (qwerty/fn* ()
    (qwerty/do
     (qwerty/. fmt.Println "Qwerty, unreleased.")
     nil)))

(qwerty/def nop
  (qwerty/fn* (x) x))

(qwerty/def eval
  (qwerty/fn* (obj)
    (qwerty/if (list? obj)
      (qwerty/let* ((op (qwerty/cast qwerty.IFn (deref (var (car obj)))))
                    (a1 (car (cdr obj)))
                    (a2 (car (cdr (cdr obj)))))
        (qwerty/go-method-call op Invoke2_1 a1 a2))
      obj)))

(qwerty/func main () ()
  (qwerty/do
   (print-notice)
   (qwerty/let* ((fd (open "/dev/stdin"))
                 (rdr (reader fd)))
     (qwerty/labels
      start
      (print-prompt)
      (qwerty/let* ((r (eval (lisp/read rdr))))
        (qwerty/do
         (qwerty/. fmt.Println (lisp/pr-str r) "::" (qwerty/. reflect.TypeOf r))
         (qwerty/goto start)))))
   ))
