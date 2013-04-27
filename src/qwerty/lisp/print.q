(qwerty/package qwerty)
(qwerty/import reflect)
(qwerty/import fmt)
(qwerty/import strconv)

(qwerty/godef pr_dispatch (qwerty/make "map[string]interface{}"))

(qwerty/godef Type (qwerty/fn* (x)
                     (qwerty/if (qwerty/nil? x)
                       "nil"
                       (qwerty/let* ((t (qwerty/cast reflect.Type (qwerty/. reflect.TypeOf x))))
                         (qwerty/go-method-call t String)))))

(qwerty/let* ((k (qwerty/cast string ((qwerty/goref Type) (qwerty/quote foo)))))
  (qwerty/map-update (qwerty/goref pr_dispatch) k
                     (qwerty/fn* (exp) (qwerty/go-method-call (qwerty/cast *ASymbol exp) String))))

(qwerty/let* ((k (qwerty/cast string ((qwerty/goref Type) (qwerty/quote (nil))))))
  (qwerty/map-update (qwerty/goref pr_dispatch) k
                     (qwerty/fn* (exp) (qwerty/go-method-call (qwerty/cast *ACons exp) String))))

(qwerty/let* ((k (qwerty/cast string ((qwerty/goref Type) 1))))
  (qwerty/map-update (qwerty/goref pr_dispatch) k
                     (qwerty/fn* (exp)
                       (qwerty/. fmt.Sprintf "%v" exp))))

(qwerty/let* ((k (qwerty/cast string ((qwerty/goref Type) ""))))
  (qwerty/map-update (qwerty/goref pr_dispatch) k
                     (qwerty/fn* (exp)
                       (qwerty/. strconv.Quote (qwerty/cast string exp)))))

(qwerty/let* ((k "*qwerty.AVar")
              (k (qwerty/cast string k)))
  (qwerty/map-update (qwerty/goref pr_dispatch) k
                     (qwerty/fn* (exp) (qwerty/go-method-call (qwerty/cast *AVar exp) String))))

(qwerty/godef string_append
  (qwerty/fn* (x y)
    (qwerty/let* ((a (qwerty/cast string x))
                  (b (qwerty/cast string y)))
      (qwerty/+ a b))))

(qwerty/godef PrStr
  (qwerty/fn* (exp)
    (qwerty/if (qwerty/nil? exp)
      "nil"
      (qwerty/let* ((k (qwerty/cast string ((qwerty/goref Type) exp))))
        (qwerty/results (value found)
          (qwerty/map-entry (qwerty/goref pr_dispatch) k)
          (qwerty/if found
            (value exp)
            ((qwerty/goref string_append)
             "#<unknown "
             ((qwerty/goref string_append)
              k
              ">"))))))))



(qwerty/def lisp/pr-str
  (qwerty/fn* (obj) ((qwerty/goref PrStr) obj)))
