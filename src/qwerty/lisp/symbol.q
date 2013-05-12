(qwerty/package qwerty)

(qwerty/struct ASymbol
  (qwerty/T name string)
  (qwerty/T hash int))

(qwerty/func Symbol_ ((qwerty/T name interface)) ((qwerty/T _ interface))
  (qwerty/let* ((n (qwerty/cast string name))
                (s (qwerty/new ASymbol))
                (h (qwerty/cast int (qwerty/. Hash n))))
    (qwerty/do
     (qwerty/set! (qwerty/.- s name) n)
     (qwerty/set! (qwerty/.- s hash) h)
     (qwerty/return s))))

(qwerty/godef Symbol (qwerty/fn* (name) (qwerty/. Symbol_ name)))

(qwerty/def symbol/name
  (qwerty/fn* (sym)
    (qwerty/let* ((s (qwerty/cast (* ASymbol) sym)))
      (qwerty/.- s name))))

(qwerty/. InternVar_
          (qwerty/. Symbol_ "qwerty/symbol")
          (qwerty/fn* (name) ((qwerty/goref Symbol) name)))

(qwerty/func (qwerty/T s ASymbol) String () ((qwerty/T _ string))
  (qwerty/let* ((s (qwerty/cast string (qwerty/.- s name))))
    (qwerty/return s)))

(qwerty/func (qwerty/T s ASymbol) HashCode  () ((qwerty/T _ interface))
  (qwerty/return (qwerty/.- s hash)))

(qwerty/func Symbol_intern1 ((qwerty/T name interface)) ((qwerty/T _ interface))
  (qwerty/return
   (qwerty/. Symbol_ name)))

(qwerty/func Symbol_intern2
  ((qwerty/T name1 interface) (qwerty/T name2 interface)) ((qwerty/T _ interface))
  (qwerty/return
   (qwerty/. Symbol_
             (qwerty/. string_concat
                       (qwerty/. string_concat name1 "/")
                       name2))))

(qwerty/def nop (qwerty/fn* (x) x))

(qwerty/def symbol?
  (qwerty/fn* (obj)
    (qwerty/results (foo ok) (qwerty/cast *ASymbol obj)
      (qwerty/do
       (nop foo)
       (qwerty/if ok
         true
         false)))))

(qwerty/def same-symbol?
  (qwerty/fn* (s1 s2)
    (qwerty/if (symbol? s1)
      (qwerty/if (symbol? s2)
        (qwerty/let* ((s1 (qwerty/cast *ASymbol s1))
                      (s2 (qwerty/cast *ASymbol s2)))
          (qwerty/= (qwerty/.- s1 name)
                    (qwerty/.- s2 name)))
        false)
      false)))
