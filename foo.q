(qwerty/package main)
(qwerty/import fmt)
(qwerty/import os)
(qwerty/import io)
(qwerty/import bufio)

(qwerty/definterface ISeq
  (first () (r))
  (rest () (r)))

(qwerty/func NOP ((qwerty/T x interface)) ((qwerty/T _ interface))
  (qwerty/return nil))

(qwerty/godef identity (qwerty/fn* (x) x))

(qwerty/godef println
  (qwerty/fn* (x)
    (qwerty/do
     (qwerty/. fmt.Println x)
     nil)))

(qwerty/def println
  (qwerty/fn* (x) ((qwerty/goref println) x)))

(qwerty/godef iadd (qwerty/fn* (x y)
                     (qwerty/let* ((a (qwerty/cast int x))
                                   (b (qwerty/cast int y)))
                       (qwerty/+ a b))))

(qwerty/godef string_append_rune (qwerty/fn* (x y)
                                   (qwerty/let* ((a (qwerty/cast string x))
                                                 (b (qwerty/cast rune y))
                                                 (c (qwerty/cast byte (qwerty/. byte b)))
                                                 (d (qwerty/cast string (qwerty/. string c))))
                                     (qwerty/+ a d))))

;; ;; (qwerty/defgofun stdin_rune_ ()
;; ;;                  (() rune)
;; ;;                  (qwerty/do
;; ;;                   (qwerty/local x rune)
;; ;;                   (qwerty/. fmt.Scanf "%c" (qwerty/goderef x))
;; ;;                   x))

;; ;; (qwerty/defgofun stdin_int_ ()
;; ;;                  (() int)
;; ;;                  (qwerty/do
;; ;;                   (qwerty/local x int)
;; ;;                   (qwerty/. fmt.Scanf "%d" (qwerty/goderef x))
;; ;;                   x))

;; ;; (qwerty/godef stdin-int (qwerty/fn* () (qwerty/. stdin_int_)))

;; ;; (qwerty/godef stdin-rune (qwerty/fn* () (qwerty/. stdin_rune_)))

(qwerty/func test1 () ()
  (qwerty/do
   (qwerty/let* ((one "one"))
     (qwerty/results (a b c) ((qwerty/fn* () (qwerty/values one "two" "three")))
       (qwerty/do
        (println a)
        (println b)
        (println c))))
   ((qwerty/fn* () (println "foo")))
   (println "x")
   ;; ((qwerty/goref println) (iadd (stdin-int)
   ;;                (stdin-int)))
   (println (qwerty/nil? nil))
   (println ((qwerty/goref iadd) 1 2))
   (println ((qwerty/goref qwerty.Cdr)
             (qwerty/. qwerty.Cons "x" "y")))
   (println "Hello World")))

(qwerty/func test2 () ()
  (qwerty/do
   (println ((qwerty/fn* () (qwerty/values 1 2))))
   (qwerty/let* ((a "foo"))
     (qwerty/labels
      start (qwerty/test (qwerty/nil? a) foo)
      (qwerty/goto end)
      foo   (qwerty/do
             (println "here")
             (qwerty/set! a nil)
             (qwerty/goto start))
      end))
   (qwerty/let* ((ch (qwerty/make "chan interface{}")))
     (qwerty/do
      (qwerty/go (qwerty/fn* ()
                   (qwerty/go<- (m ch)
                     (qwerty/do
                      (println m)))))
      (qwerty/go-> ("Hello" ch)
        (println "sent"))))))

(qwerty/godef open (qwerty/fn* (file_name)
                     (qwerty/let* ((fn (qwerty/cast string file_name)))
                       (qwerty/results (fd ok) (qwerty/. os.Open fn)
                         (qwerty/if (qwerty/nil? ok)
                           fd
                           (qwerty/do
                            (qwerty/. panic ok)
                            nil))))))

(qwerty/godef reader (qwerty/fn* (fd)
                       (qwerty/let* ((fd (qwerty/cast io.Reader fd)))
                         (qwerty/. bufio.NewReader fd))))

(qwerty/def panic (qwerty/fn* (err) (qwerty/do (qwerty/. panic err) nil)))

(qwerty/godef foo (qwerty/. qwerty.InternVar_ (qwerty/. qwerty.Symbol_ "foo") "Hello Var World"))

(qwerty/godef deref (qwerty/fn* (v)
                      (qwerty/let* ((v (qwerty/cast *qwerty.AVar v)))
                        (qwerty/go-method-call v Deref))))

(println "here1111111")
(println ((qwerty/goref deref) ((qwerty/goref qwerty.Var) (qwerty/quote lisp/first))))

(qwerty/func test3 () ()
  (qwerty/do
   (println "test3")
   (println ((qwerty/goref deref) ((qwerty/goref qwerty.Var) (qwerty/quote a/b))))
   (println ((qwerty/goref deref) (qwerty/goref foo)))
   (println (lisp/pr-str (qwerty/quote foo/bar)))
   ;; (qwerty/let* ((readd (qwerty/fn* (read read_list)
   ;;                        (qwerty/let*
   ;;                            ((read_listd (qwerty/fn* () (read_list read read_list))))
   ;;                          (qwerty/fn*
   ;;                              (rdr)
   ;;                            (qwerty/let*
   ;;                                ((read_list (read_listd)))
   ;;                              (qwerty/let*
   ;;                                  ((rdr (qwerty/cast *bufio.Reader rdr)))
   ;;                                (qwerty/results (b size err) (qwerty/go-method-call rdr ReadRune)
   ;;                                                (qwerty/do
   ;;                                                 ((qwerty/goref identity) err)
   ;;                                                 ((qwerty/goref identity) size)
   ;;                                                 (qwerty/let* ((bar (qwerty/if (qwerty/nil? err)
   ;;                                                                      nil
   ;;                                                                      (panic err))))
   ;;                                                   (qwerty/. NOP bar))
   ;;                                                 (qwerty/if (qwerty/= b \()
   ;;                                                   (qwerty/do
   ;;                                                    (read_list rdr))
   ;;                                                   (panic
   ;;                                                    ((qwerty/goref string_append_rune)
   ;;                                                     "unknown rune read " b)))))))))))
   ;;               (read_listd (qwerty/fn*
   ;;                               (read read_list)
   ;;                             (qwerty/let* ((readd (qwerty/fn* () (read read read_list))))
   ;;                               (qwerty/fn* (rdr)
   ;;                                 (qwerty/let* ((read (readd)))
   ;;                                   (qwerty/do
   ;;                                    (read rdr)))))))
   ;;               (read (readd readd read_listd))
   ;;               (fd ((qwerty/goref open) "./foo.q"))
   ;;               (rdr ((qwerty/goref reader) fd)))
   ;;   ((qwerty/goref println) (read rdr)))
   ))

(qwerty/def FOO "Hello World")

(qwerty/def X (qwerty/fn* () ((qwerty/goref println) FOO)))

(qwerty/func main () ()
  (qwerty/do
   ((qwerty/fn* () (println "function call")))
   (qwerty/defer (qwerty/fn* () (println "derefed action")))
   (println "hash of foo")
   (println (qwerty/. qwerty.Hash "foo"))
   (println (qwerty/. qwerty.Hash (qwerty/quote foo)))
   (println "main")
   (println ((qwerty/goref deref) ((qwerty/goref qwerty.Var) (qwerty/quote qwerty/first))))
   (qwerty/. test2)
   (qwerty/. test1)
   (println "Printing Var")
   (println (lisp/pr-str ((qwerty/goref qwerty.Var) (qwerty/quote qwerty/first))))
   (println "fold")
   (println (lisp/fold (qwerty/goref iadd) 0 (qwerty/quote (1 2 3 4 5))))
   (X)
   (println "PrStr")
   (println (lisp/pr-str (qwerty/quote (3 4 a b c "x\"y\"\n" (1 2) nil))))
   ;; (println "Apply")
   ;; (println (qwerty/go-method-call (qwerty/goref iadd) Apply1 (qwerty/quote (1 2))))
   (qwerty/. test3)))


;; type expressions

;; (T x interface)
;; (T y int)
;; (T x (* Symbol))
