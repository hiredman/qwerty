(qwerty/package qwerty)
(qwerty/import fmt)
(qwerty/import reflect)

(qwerty/def library (qwerty/make (map string interface)))

(qwerty/let* ((l (qwerty/cast (map string interface) library)))
  (qwerty/do
   (qwerty/map-update l "fmt.Println" (qwerty/goref fmt.Println))
   (qwerty/map-update l "reflect.TypeOf" (qwerty/goref reflect.TypeOf))
   (qwerty/map-update l "reflect.ValueOf" (qwerty/goref reflect.ValueOf))
   ))

(qwerty/def reflective/invoke
  (qwerty/fn* (fun args)
    (qwerty/let* ((i (qwerty/cast int ((qwerty/goref ListCount) args)))
                  (v (qwerty/make (slice reflect.Value) i))
                  (r args)
                  (ii 0)
                  (sn (qwerty/cast string (symbol/name fun)))
                  (m (qwerty/cast (map string interface) library))
                  (f (qwerty/cast reflect.Value
                                  (qwerty/. reflect.ValueOf (qwerty/map-entry m sn)))))
      (qwerty/do
       (qwerty/labels
        start
        (qwerty/test (qwerty/= ii i) continue)
        (qwerty/goto end)
        continue
        (qwerty/let* ((a (qwerty/cast reflect.Value (qwerty/. reflect.ValueOf (car r)))))
          (qwerty/do
           (qwerty/set! (qwerty/aget (qwerty/cast (slice reflect.Value) v)
                                     (qwerty/cast int ii))
                        a)
           (qwerty/set! ii (qwerty/. iadd ii 1))
           (qwerty/set! r (cdr r))
           (qwerty/goto start)))
        end)
       (qwerty/let* ((v (qwerty/cast (slice reflect.Value) v))
                     (r (qwerty/go-method-call f Call v)))
         r)))))
