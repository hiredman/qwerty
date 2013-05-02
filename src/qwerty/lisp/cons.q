(qwerty/package qwerty)

(qwerty/struct ACons
  (qwerty/T car interface)
  (qwerty/T cdr interface)
  (qwerty/T hash int))

;; [[_ name args returns body]]
;; [[_ target name args returns body]]

(qwerty/func Cons ((qwerty/T x interface)
                   (qwerty/T y interface))
  ((qwerty/T _ interface))
  (qwerty/let* ((c (qwerty/new ACons)))
    (qwerty/do
     (qwerty/set! (qwerty/.- c car) x)
     (qwerty/set! (qwerty/.- c cdr) y)
     (qwerty/return c))))

(qwerty/func (qwerty/T c ACons) Car () ((qwerty/T _ interface))
  (qwerty/return (qwerty/.- c car)))

(qwerty/func CarF ((qwerty/T c interface)) ((qwerty/T _ interface))
  (qwerty/return
   (qwerty/if (qwerty/nil? c)
     nil
     (qwerty/results (foo ok) (qwerty/cast *ACons c)
       (qwerty/if ok
         (qwerty/let* ((foo (qwerty/cast *ACons foo)))
           (qwerty/go-method-call foo Car))
         (qwerty/let* ((foo (qwerty/cast ACons c)))
           (qwerty/go-method-call foo Car)))))))

(qwerty/func CdrF ((qwerty/T c interface)) ((qwerty/T _ interface))
  (qwerty/return
   (qwerty/if (qwerty/nil? c)
     nil
     (qwerty/results (foo ok) (qwerty/cast *ACons c)
       (qwerty/if ok
         (qwerty/let* ((foo (qwerty/cast *ACons foo)))
           (qwerty/.- foo cdr))
         (qwerty/let* ((foo (qwerty/cast ACons c)))
           (qwerty/.- foo cdr)))))))


(qwerty/godef Car (qwerty/fn* (c) (qwerty/. CarF c)))

(qwerty/godef Cdr (qwerty/fn* (c) (qwerty/. CdrF c)))

(qwerty/godef ListCount (qwerty/fn* (lst)
                          (qwerty/if (qwerty/nil? lst)
                            0
                            (qwerty/. iadd 1 ((qwerty/goref ListCount) ((qwerty/goref Cdr) lst))))))

(qwerty/def cons
  (qwerty/fn* (a b) (qwerty/. Cons a b)))

(qwerty/def lisp/map
  (qwerty/fn* (f lst)
    (qwerty/if (qwerty/nil? lst)
      nil
      ((qwerty/goref Cons)
       (f ((qwerty/goref Car) lst))
       (lisp/map f ((qwerty/goref Cdr) lst))))))

;; (qwerty/def lisp/fold
;;   (qwerty/fn* (f init lst)
;;     (qwerty/do
;;      (qwerty/labels
;;       start
;;       (qwerty/test (qwerty/nil? lst) reduce)
;;       (qwerty/goto end)
;;       reduce
;;       (qwerty/do
;;        (qwerty/set! init (f init ((qwerty/goref Car) lst)))
;;        (qwerty/set! lst ((qwerty/goref Cdr) lst))
;;        (qwerty/goto start))
;;       end)
;;      init)))

(qwerty/def lisp/fold
  (qwerty/fn* (f init lst)
    (qwerty/if (qwerty/nil? lst)
      init
      (lisp/fold f (f init ((qwerty/goref Car) lst))
                 ((qwerty/goref Cdr) lst)))))


(qwerty/def lisp/reverse
  (qwerty/fn* (lst) (lisp/fold (qwerty/fn* (a b) (cons b a)) nil lst)))


;; (qwerty/defgomethod HashCode ACons (s) (r)
;;   (() (interface))
;;   (qwerty/do
;;    (qwerty/. panic "hashing cons")
;;    0))

(qwerty/def nop (qwerty/fn* (x) x))

;; ;; this is so gross
(qwerty/func (qwerty/T s ACons) String () ((qwerty/T _ string))
  (qwerty/let* ((r (qwerty/let* ((r "(")
                                 (lst s))
                     (qwerty/do
                      (qwerty/labels
                       (qwerty/goto item)
                       space
                       (qwerty/set! r ((qwerty/goref string_append) r " "))
                       item
                       (qwerty/let* ((first ((qwerty/goref Car) lst))
                                     (rest ((qwerty/goref Cdr) lst)))
                         (qwerty/do
                          (nop
                           (qwerty/if (qwerty/= rest nil)
                             (qwerty/do
                              (qwerty/set! r ((qwerty/goref string_append) r ((qwerty/goref PrStr) first)))
                              (qwerty/goto end)
                              nil)
                             (qwerty/do
                              (qwerty/set! r ((qwerty/goref string_append) r ((qwerty/goref PrStr) first)))
                              (qwerty/set! lst rest)
                              (qwerty/goto space)
                              nil)))))
                       end)
                      (qwerty/set! r ((qwerty/goref string_append) r ")"))
                      r)))
                (r (qwerty/cast string r)))
    (qwerty/return r)))
