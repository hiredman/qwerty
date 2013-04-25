(qwerty/package qwerty)
(qwerty/import reflect)
#_(qwerty/import fmt)

(qwerty/godef pr_dispatch (qwerty/make "map[string]interface{}"))

(qwerty/godef Type (qwerty/fn* (x)
                               (qwerty/if (qwerty/nil? x)
                                 "nil"
                                 (qwerty/let* ((t (qwerty/cast reflect.Type (qwerty/. reflect.TypeOf x))))
                                              (qwerty/go-method-call t String)))))

(qwerty/let* ((k (qwerty/cast string ((qwerty/goref Type) (qwerty/quote foo)))))
             (qwerty/map-update (qwerty/goref pr_dispatch) k
                                (qwerty/fn* (exp) (qwerty/go-method-call (qwerty/cast *ASymbol exp) String)))
             nil)

(qwerty/godef PrStr
              (qwerty/fn* (exp)
                          (qwerty/let* ((k (qwerty/cast string ((qwerty/goref Type) exp))))
                                       (qwerty/results (value found)
                                                       (qwerty/map-entry (qwerty/goref pr_dispatch) k)
                                                       (qwerty/if found
                                                         (value exp)
                                                         "#<unknown>")))))

;; (qwerty/godef deref (qwerty/fn* (v) (qwerty/go-method-call (qwerty/cast *AVar v) Deref)))

;; ;; example of what qwerty/def should expand in to
;; (qwerty/godef _
;;               (InternVar (Symbol "qwerty/pr")
;;                          (qwerty/let* ((mapV (Var (Symbol "qwerty/pr"))))
;;                                       (qwerty/fn* ()
;;                                                   (qwerty/if (qwerty/nil? lst)
;;                                                     nil
;;                                                     (Cons (f (Car lst))
;;                                                           ((deref mapV) f (Cdr lst))))))))

