(qwerty/package main)
(qwerty/import "fmt")
(qwerty/import "os")
(qwerty/import "io")
(qwerty/import "bufio")
;; (qwerty/import "types")

(qwerty/struct Cons
               car interface
               cdr interface)

(qwerty/struct Var
               name string
               value interface
               macro bool)

(qwerty/definterface IFn

                     (invoke0_1 () (result1))
                     (invoke0_2 () (result1 result2))
                     (invoke0_3 () (result1 result2 result3))
                     (invoke0_4 () (result1 result2 result3 result4))

                     (invoke1_1 (a1) (result1))
                     (invoke1_2 (a1) (result1 result2))
                     (invoke1_3 (a1) (result1 result2 result3))
                     (invoke1_4 (a1) (result1 result2 result3 result4))

                     (invoke2_1 (a1 a2) (result1))
                     (invoke2_2 (a1 a2) (result1 result2))
                     (invoke2_3 (a1 a2) (result1 result2 result3))
                     (invoke2_4 (a1 a2) (result1 result2 result3 result4))

                     (invoke3_1 (a1 a2 a3) (result1))
                     (invoke3_2 (a1 a2 a3) (result1 result2))
                     (invoke3_3 (a1 a2 a3) (result1 result2 result3))
                     (invoke3_4 (a1 a2 a3) (result1 result2 result3 result4))

                     (invoke4_1 (a1 a2 a3 a4) (result1))
                     (invoke4_2 (a1 a2 a3 a4) (result1 result2))
                     (invoke4_3 (a1 a2 a3 a4) (result1 result2 result3))
                     (invoke4_4 (a1 a2 a3 a4) (result1 result2 result3 result4))

                     )

(qwerty/definterface ISeq
                     (first () (r))
                     (rest () (r)))

(qwerty/defgofun NOP (arg)
                 ((interface) interface)
                 nil)


(qwerty/godef the_vars (qwerty/make "map[string]Var"))

(qwerty/godef println
              (qwerty/fn* (x)
                          (qwerty/do
                           (qwerty/. fmt.Println x)
                           nil)))

(qwerty/godef make_var (qwerty/fn* (name value)
                                   (qwerty/let* ((n (qwerty/cast string name))
                                                 (m (qwerty/cast "map[string]Var" the_vars)))
                                                (qwerty/results (value found) (qwerty/map-entry m n)
                                                                (println
                                                                 (qwerty/if found
                                                                            (println value)
                                                                            (qwerty/if (qwerty/nil? nil)
                                                                                       (println "it is nil!")
                                                                                       (println "not-found"))))))))


(qwerty/godef cons (qwerty/fn* (x y)
                               (qwerty/let* ((c (qwerty/new Cons)))
                                            (qwerty/do
                                             (qwerty/set! (qwerty/.- c car) x)
                                             (qwerty/set! (qwerty/.- c cdr) y)
                                             c))))

(qwerty/godef car (qwerty/fn* (c)
                              (qwerty/let* ((foo (qwerty/cast *Cons c)))
                                           (qwerty/.- foo car))))

(qwerty/godef cdr (qwerty/fn* (c)
                              (qwerty/let* ((foo (qwerty/cast *Cons c)))
                                           (qwerty/.- foo cdr))))

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

(qwerty/defgofun stdin_rune_ ()
                 (() rune)
                 (qwerty/do
                  (qwerty/local x rune)
                  (qwerty/. fmt.Scanf "%c" (qwerty/goderef x))
                  x))

(qwerty/defgofun stdin_int_ ()
                 (() int)
                 (qwerty/do
                  (qwerty/local x int)
                  (qwerty/. fmt.Scanf "%d" (qwerty/goderef x))
                  x))

(qwerty/godef stdin-int (qwerty/fn* () (qwerty/. stdin_int_)))

(qwerty/godef stdin-rune (qwerty/fn* () (qwerty/. stdin_rune_)))

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
                  (println (cdr (cons "x" "y")))
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
                                             (println "sent"))))
                  (make_var "foo" 1)))

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

;; (qwerty/godef read_list_fn
;;               (qwerty/fn* (read)
;;                           (qwerty/fn* (rdr)
;;                                       (read rdr))))

;; (qwerty/godef )

;; (qwerty/godef read_fn
;;               (qwerty/fn* (read_list)
;;                           (qwerty/fn* (reader)
;;                                       (qwerty/let* ((rdr (qwerty/cast *bufio.Reader reader)))
;;                                                    (qwerty/do
;;                                                     (qwerty/results (b size err) (qwerty/go-method-call rdr ReadRune)
;;                                                                     (qwerty/do
;;                                                                      (qwerty/. NOP err)
;;                                                                      (qwerty/. NOP size)
;;                                                                      (qwerty/let* ((foo (qwerty/if (qwerty/nil? err)
;;                                                                                                    nil
;;                                                                                                    (raise err))))
;;                                                                                   (qwerty/. NOP foo))
;;                                                                      (qwerty/if (qwerty/= b \()
;;                                                                                 (qwerty/do
;;                                                                                  ;;(qwerty/go-method-call rdr UnreadRune)
;;                                                                                  (read_list read rdr))
;;                                                                                 (raise "foo")))))))))

(qwerty/defgofun main ()
                 (())
                 (qwerty/do
                  (qwerty/. test2)
                  (qwerty/. test1)
                  ((qwerty/fn* (x) ((qwerty/fn* (y) (println y)) y)) "Hello World")
                  #_(qwerty/let* ((read (qwerty/fn* (read read_list)
                                                  (qwerty/let* ((read (qwerty/fn* () (read read read_list)))
                                                                (read_list (qwerty/fn* () (read_list read read_list))))
                                                               (qwerty/fn* (rdr)
                                                                           (qwerty/let* ((read (read))
                                                                                         (read_list (read_list)))
                                                                                        (qwerty/if (qwerty/nil? rdr)
                                                                                                   nil
                                                                                                   (read_list rdr)))))))
                                (read_list (qwerty/fn* (read read_list)
                                                       (qwerty/let* ((read (qwerty/fn* () (read read read_list)))
                                                                     (read_list (qwerty/fn* () (read_list read read_list))))
                                                                    (qwerty/fn* (rdr)
                                                                                (qwerty/let* ((read (read))
                                                                                              (read_list (read_list)))
                                                                                             (qwerty/do
                                                                                              (println "read_list")
                                                                                              (read nil))))))))
                               (qwerty/let* ((read (qwerty/fn* () (read read read_list)))
                                             (read_list (qwerty/fn* () (read_list read read_list)))
                                             (read (read))
                                             (read_list (read_list)))
                                            (read "foo")))
                  (qwerty/let* ((fd (open "./foo.lisp"))
                                (rdr (reader fd)))
                               (read rdr))))


;; (make-channel x)

;; (qwerty/select
;;  (qwerty/go<- ((i3,ok) ch)
;;               body)

;;  (qwerty/go-> (value ch)
;;               body))
