(qwerty/package main)
(qwerty/import fmt)
;; (qwerty/import reflect)

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

(qwerty/func main () ()
  (qwerty/do
   (print-notice)
   (qwerty/let* ((fd (open "/dev/stdin"))
                 (rdr (reader fd)))
     (qwerty/labels
      start
      (print-prompt)
      (qwerty/let* ((r (lisp/read rdr)))
        (qwerty/do
         (println (lisp/pr-str r))
         (qwerty/goto start)))))
   ))
