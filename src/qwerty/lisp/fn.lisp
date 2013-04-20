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

  )
