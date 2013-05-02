(qwerty/package qwerty)

(qwerty/definterface IFn

  (Invoke0_1 () (result1))
  (Invoke0_2 () (result1 result2))
  (Invoke0_3 () (result1 result2 result3))
  (Invoke0_4 () (result1 result2 result3 result4))

  (Invoke1_1 (a1) (result1))
  (Invoke1_2 (a1) (result1 result2))
  (Invoke1_3 (a1) (result1 result2 result3))
  (Invoke1_4 (a1) (result1 result2 result3 result4))

  (Invoke2_1 (a1 a2) (result1))
  (Invoke2_2 (a1 a2) (result1 result2))
  (Invoke2_3 (a1 a2) (result1 result2 result3))
  (Invoke2_4 (a1 a2) (result1 result2 result3 result4))

  (Invoke3_1 (a1 a2 a3) (result1))
  (Invoke3_2 (a1 a2 a3) (result1 result2))
  (Invoke3_3 (a1 a2 a3) (result1 result2 result3))
  (Invoke3_4 (a1 a2 a3) (result1 result2 result3 result4))

  (Invoke4_1 (a1 a2 a3 a4) (result1))
  (Invoke4_2 (a1 a2 a3 a4) (result1 result2))
  (Invoke4_3 (a1 a2 a3 a4) (result1 result2 result3))
  (Invoke4_4 (a1 a2 a3 a4) (result1 result2 result3 result4))

  (Invoke5_1 (a1 a2 a3 a4 a5) (result1))
  (Invoke5_2 (a1 a2 a3 a4 a5) (result1 result2))
  (Invoke5_3 (a1 a2 a3 a4 a5) (result1 result2 result3))
  (Invoke5_4 (a1 a2 a3 a4 a5) (result1 result2 result3 result4))

  (Invoke6_1 (a1 a2 a3 a4 a5 a6) (result1))
  (Invoke6_2 (a1 a2 a3 a4 a5 a6) (result1 result2))
  (Invoke6_3 (a1 a2 a3 a4 a5 a6) (result1 result2 result3))
  (Invoke6_4 (a1 a2 a3 a4 a5 a6) (result1 result2 result3 result4))

  (Apply1 (args) (result1))
  (Apply2 (args) (result1 result2))
  (Apply3 (args) (result1 result2 result3))
  (Apply4 (args) (result1 result2 result3 result4))

  )


(qwerty/def lisp/apply
  (qwerty/fn* (f args)
    (qwerty/let* ((op (qwerty/cast IFn f)))
      (qwerty/go-method-call op Apply1 args))))
