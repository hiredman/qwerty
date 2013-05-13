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

(qwerty/def new-env
  (qwerty/fn* ()
    (cons (qwerty/make (map string interface)) nil)))

(qwerty/def look-up
  (qwerty/fn* (n env)
    (qwerty/if (qwerty/nil? env)
      nil
      (qwerty/let* ((n (qwerty/cast string (symbol/name n)))
                    (ge (qwerty/cast (map string interface) (car env))))
        (qwerty/results (item found?) (qwerty/map-entry ge n)
          (qwerty/if found?
            (cons n item)
            (look-up n (cdr env))))))))

(qwerty/def bind
  (qwerty/fn* (env n value)
    (qwerty/if (qwerty/nil? (cdr env))
      (qwerty/do
       (qwerty/let* ((n (qwerty/cast string (symbol/name n)))
                     (ge (qwerty/cast (map string interface) (car env))))
         (qwerty/map-update ge n value))
       nil)
      (qwerty/let* ((n (qwerty/cast string (symbol/name n)))
                    (ge (qwerty/cast (map string interface) (car env))))
        (qwerty/results (item found?) (qwerty/map-entry ge n)
          (qwerty/if found?
            (qwerty/do
             (nop item)
             (qwerty/map-update ge n value)
             nil)
            (bind (cdr env) n value)))))))

(qwerty/def global-env (new-env))
(qwerty/def *package* (qwerty/quote qwerty))

(qwerty/def go/eval*
  (qwerty/fn* (obj env)
    (qwerty/if (list? obj)
      (qwerty/if (same-symbol? (car obj) (qwerty/quote qwerty/godef))
        (qwerty/let* ((n (car (cdr obj)))
                      (v (go/eval* (car (cdr (cdr obj))) env)))
          (qwerty/do
           (bind env n v)
           nil))
        (qwerty/if (same-symbol? (car obj) (qwerty/quote qwerty/do))
          (lisp/fold
           (qwerty/fn* (lv obj)
             (go/eval* obj env))
           nil
           (cdr obj))
          nil))
      (qwerty/if (symbol? obj)
        (qwerty/let* ((n (look-up obj env)))
          (qwerty/if (qwerty/nil? n)
            (qwerty/do
             (qwerty/. panic "dunno")
             nil)
            (cdr n)))
        obj))))

(qwerty/def go/eval
  (qwerty/fn* (obj)
    (go/eval* obj global-env)))

(qwerty/def eval
  (qwerty/fn* (obj)
    (qwerty/if (list? obj)
      (qwerty/if (same-symbol? (car obj) (qwerty/quote qwerty/def))
        (qwerty/let* ((n (car (cdr obj)))
                      (v (car (cdr (cdr obj)))))
          (qwerty/. qwerty.InternVar_ n v))
        (lisp/apply (eval (car obj))
                    (lisp/map eval (cdr obj))))
      (qwerty/if (symbol? obj)
        (deref (var obj))
        obj))))

(qwerty/func main () ()
  (qwerty/do
   (print-notice)
   (qwerty/let* ((fd (open "/dev/stdin"))
                 (rdr (reader fd)))
     (qwerty/labels
      start
      (print-prompt)
      (qwerty/let* ((r (go/eval (lisp/read rdr))))
        (qwerty/do
         (qwerty/. fmt.Println (lisp/pr-str r) "::" (qwerty/. reflect.TypeOf r))
         (qwerty/goto start)))))))
