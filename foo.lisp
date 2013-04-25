(qwerty/package main)
(qwerty/import fmt)
(qwerty/import os)
(qwerty/import io)
(qwerty/import bufio)

(qwerty/definterface ISeq
  (first () (r))
  (rest () (r)))

(qwerty/defgofun NOP (arg)
  ((interface) interface)
  nil)

(qwerty/godef identity (qwerty/fn* (x) x))

(qwerty/godef println
              (qwerty/fn* (x)
                          (qwerty/do
                            (qwerty/. fmt.Println x)
                            nil)))

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

(qwerty/defgofun test1 ()
  (())
  (qwerty/do
    (qwerty/let* ((one "one"))
                 (qwerty/results (a b c) ((qwerty/fn* () (qwerty/values one "two" "three")))
                                 (qwerty/do
                                   ((qwerty/goref println) a)
                                   ((qwerty/goref println) b)
                                   ((qwerty/goref println) c))))
    ((qwerty/fn* () ((qwerty/goref println) "foo")))
    ((qwerty/goref println) "x")
    ;; ((qwerty/goref println) (iadd (stdin-int)
    ;;                (stdin-int)))
    ((qwerty/goref println) (qwerty/nil? nil))
    ((qwerty/goref println) ((qwerty/goref iadd) 1 2))
    ((qwerty/goref println) ((qwerty/goref qwerty.Cdr)
                             ((qwerty/goref qwerty.Cons) "x" "y")))
    ((qwerty/goref println) "Hello World")))

(qwerty/defgofun test2 ()
  (())
  (qwerty/do
    ((qwerty/goref println) ((qwerty/fn* () (qwerty/values 1 2))))
    (qwerty/let* ((a "foo"))
                 (qwerty/labels
                  start (qwerty/test (qwerty/nil? a) foo)
                  (qwerty/goto end)
                  foo   (qwerty/do
                          ((qwerty/goref println) "here")
                          (qwerty/set! a nil)
                          (qwerty/goto start))
                  end))
    (qwerty/let* ((ch (qwerty/make "chan interface{}")))
                 (qwerty/do
                   (qwerty/go (qwerty/fn* ()
                                          (qwerty/go<- (m ch)
                                                       (qwerty/do
                                                         ((qwerty/goref println) m)))))
                   (qwerty/go-> ("Hello" ch)
                                ((qwerty/goref println) "sent"))))))

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

(qwerty/godef raiseS (qwerty/. qwerty.Symbol_ "raise"))

(qwerty/godef raiseV (qwerty/. qwerty.InternVar_ (qwerty/goref raiseS)
                               (qwerty/fn* (err)
                                           (qwerty/do
                                             (qwerty/. panic err)
                                             nil))))

(qwerty/godef foo (qwerty/. qwerty.InternVar_ (qwerty/. qwerty.Symbol_ "foo") "Hello Var World"))

(qwerty/godef deref (qwerty/fn* (v)
                                (qwerty/let* ((v (qwerty/cast *qwerty.AVar v)))
                                             (qwerty/go-method-call v Deref))))

((qwerty/goref println) "here1111111")
((qwerty/goref println) ((qwerty/goref deref) ((qwerty/goref qwerty.Var) (qwerty/quote qwerty/first))))

(qwerty/defgofun test3 ()
  (())
  (qwerty/do
    ((qwerty/goref println) ((qwerty/goref deref) ((qwerty/goref qwerty.Var) (qwerty/quote a/b))))
    ((qwerty/goref println) ((qwerty/goref deref) (qwerty/goref foo)))
    ((qwerty/goref println) ((qwerty/goref qwerty.Symbol) "foo/bar"))
    (qwerty/let* ((readd (qwerty/fn*
                          (read read_list)
                          (qwerty/let*
                           ((read_listd (qwerty/fn* () (read_list read read_list))))
                           (qwerty/fn*
                            (rdr)
                            (qwerty/let*
                             ((read_list (read_listd)))
                             (qwerty/let*
                              ((rdr (qwerty/cast *bufio.Reader rdr)))
                              (qwerty/results (b size err) (qwerty/go-method-call rdr ReadRune)
                                              (qwerty/do
                                                ((qwerty/goref identity) err)
                                                ((qwerty/goref identity) size)
                                                (qwerty/let* ((bar (qwerty/if (qwerty/nil? err)
                                                                     nil
                                                                     (((qwerty/goref deref)
                                                                       (qwerty/goref raiseV)) err))))
                                                             (qwerty/. NOP bar))
                                                (qwerty/if (qwerty/= b \()
                                                  (qwerty/do
                                                    (read_list rdr))
                                                  (((qwerty/goref deref) (qwerty/goref raiseV))
                                                   ((qwerty/goref string_append_rune) "unknown rune read " b)))))))))))
                  (read_listd (qwerty/fn*
                               (read read_list)
                               (qwerty/let* ((readd (qwerty/fn* () (read read read_list))))
                                            (qwerty/fn* (rdr)
                                                        (qwerty/let* ((read (readd)))
                                                                     (qwerty/do
                                                                       (read rdr)))))))
                  (read (readd readd read_listd))
                  (fd ((qwerty/goref open) "./foo.lisp"))
                  (rdr ((qwerty/goref reader) fd)))
                 ((qwerty/goref println) (read rdr)))))

(qwerty/def FOO "Hello World")

(qwerty/def X (qwerty/fn* () ((qwerty/goref println) FOO)))

(qwerty/defgofun main ()
  (())
  (qwerty/do
    ((qwerty/goref println) "hash of foo")
    ((qwerty/goref println) (qwerty/. qwerty.Hash "foo"))
    ((qwerty/goref println) (qwerty/. qwerty.Hash (qwerty/quote foo)))
    ((qwerty/goref println) "main")
    ((qwerty/goref println) ((qwerty/goref deref) ((qwerty/goref qwerty.Var) (qwerty/quote qwerty/first))))
    (qwerty/. test2)
    (qwerty/. test1)
    ((qwerty/goref println) "Printing Var")
    ((qwerty/goref println) ((qwerty/goref qwerty.Var) (qwerty/quote qwerty/map)))
    ((qwerty/goref println) ((qwerty/goref qwerty.Car) (qwerty/quote ("first of list" "second of list"))))
    ((qwerty/goref println) "PrStr")
    ((qwerty/goref println) ((qwerty/goref qwerty.PrStr) (qwerty/quote foo)))
    ((qwerty/goref println) "fold")
    ((qwerty/goref println) (lisp/fold (qwerty/goref iadd) 0 (qwerty/quote (1 2 3 4 5))))
    (X)
    (qwerty/. test3)))
