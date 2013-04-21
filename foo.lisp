(qwerty/package main)
(qwerty/import "fmt")
(qwerty/import "os")
(qwerty/import "io")
(qwerty/import "bufio")

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
                                   (println a)
                                   (println b)
                                   (println c))))
    ((qwerty/fn* () (println "foo")))
    (println "x")
    ;; (println (iadd (stdin-int)
    ;;                (stdin-int)))
    (println (qwerty/nil? nil))
    (println (iadd 1 2))
    (println (qwerty.Cdr (qwerty.Cons "x" "y")))
    (println "Hello World")))

(qwerty/defgofun test2 ()
  (())
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

(qwerty/godef raise (qwerty/fn* (err)
                                (qwerty/do
                                  (qwerty/. panic err)
                                  nil)))

(qwerty/set! foo (qwerty/. qwerty.InternVar (qwerty/. qwerty.Symbol "foo") "Hello Var World"))

(qwerty/defgofun main ()
  (())
  (qwerty/do
    (qwerty/. test2)
    (qwerty/. test1)
    (println (qwerty.Deref (qwerty/. qwerty.Var (qwerty/. qwerty.Symbol "foo"))))
    (println (qwerty.Deref foo))
    (println (qwerty/. qwerty.Symbol "foo/bar"))
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
                                                (identity err)
                                                (identity size)
                                                (qwerty/let* ((foo (qwerty/if (qwerty/nil? err)
                                                                     nil
                                                                     (raise err))))
                                                             (qwerty/. NOP foo))
                                                (qwerty/if (qwerty/= b \()
                                                  (qwerty/do
                                                    (read_list rdr))
                                                  (raise (string_append_rune "unknown rune read " b)))))))))))
                  (read_listd (qwerty/fn*
                               (read read_list)
                               (qwerty/let* ((readd (qwerty/fn* () (read read read_list))))
                                            (qwerty/fn* (rdr)
                                                        (qwerty/let* ((read (readd)))
                                                                     (qwerty/do
                                                                       (read rdr)))))))
                  (read (readd readd read_listd))
                  (fd (open "./foo.lisp"))
                  (rdr (reader fd)))
                 (println (read rdr)))))
