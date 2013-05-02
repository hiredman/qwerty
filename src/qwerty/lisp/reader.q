(qwerty/package qwerty)
(qwerty/import io)
(qwerty/import bufio)
(qwerty/import regexp)
(qwerty/import os)
(qwerty/import text/scanner)

(qwerty/def lisp/read-list nil)
(qwerty/def lisp/read-atom nil)

(qwerty/def r/one-rune
  (qwerty/fn* (rdr)
    (qwerty/let* ((rdr (qwerty/cast *bufio.Reader rdr)))
      (qwerty/results (r size err) (qwerty/go-method-call rdr ReadRune)
        (qwerty/if (qwerty/nil? err)
          r
          (qwerty/do
           (qwerty/. panic err)
           size))))))

(qwerty/def r/unread
  (qwerty/fn* (rdr)
    (qwerty/let* ((rdr (qwerty/cast *bufio.Reader rdr)))
      (qwerty/go-method-call rdr UnreadRune))))

(qwerty/def nop
  (qwerty/fn* (x) x))

(qwerty/def lisp/read
  (qwerty/fn* (rdr)
    (qwerty/let* ((result nil)
                  (r (r/one-rune rdr)))
      (qwerty/do
       (qwerty/labels
        start
        (qwerty/test (qwerty/= r \() space)
        (qwerty/do
         (qwerty/set! result (lisp/read-list rdr))
         (qwerty/goto end))
        space
        (qwerty/test (qwerty/= r \space) atom)
        (qwerty/do
         (qwerty/set! r (r/one-rune rdr))
         (qwerty/goto start))
        atom
        (qwerty/do
         (qwerty/set! result (lisp/read-atom rdr))
         (qwerty/goto end))
        end)
       result))))

(qwerty/def lisp/read-list
  (qwerty/fn* (rdr)
    (qwerty/let* ((lst nil)
                  (rune (r/one-rune rdr)))
      (qwerty/do
       (qwerty/labels
        start
        (nop
         (qwerty/if (qwerty/= rune \))
           (qwerty/do
            (qwerty/set! lst (lisp/reverse lst))
            nil)
           (qwerty/do
            (r/unread rdr)
            (qwerty/set! lst (cons (lisp/read rdr) lst))
            (qwerty/set! rune (r/one-rune rdr))
            (qwerty/goto start)
            nil))))
       lst))))


(qwerty/def lisp/read-atom
  (qwerty/fn* (rdr)
    (qwerty/let* ((lst nil)
                  (rune (r/one-rune rdr)))
      (qwerty/if (qwerty/= rune \))
        (lisp/reverse lst)))))


(qwerty/func Pattern_compile1 ((qwerty/T p string)) ((qwerty/T _ interface))
  (qwerty/results (rexep error) (qwerty/. regexp.Compile p)
    (qwerty/return
     (qwerty/if (qwerty/nil? error)
       rexep
       (qwerty/do
        (qwerty/. panic error)
        nil)))))

(qwerty/func Var_create1 ((qwerty/T sym interface)) ((qwerty/T _ interface))
  (qwerty/return (qwerty/. Var_ sym)))

(qwerty/func SetDynamic0 ((qwerty/T sym interface)) ((qwerty/T _ interface))
  (qwerty/return nil))

(qwerty/def open
  (qwerty/fn* (file_name)
    (qwerty/let* ((fn (qwerty/cast string file_name)))
      (qwerty/results (fd ok) (qwerty/. os.Open fn)
        (qwerty/if (qwerty/nil? ok)
          fd
          (qwerty/do
           (qwerty/. panic ok)
           nil))))))

(qwerty/def reader
  (qwerty/fn* (fd)
    (qwerty/let* ((fd (qwerty/cast io.Reader fd)))
      (qwerty/. bufio.NewReader fd))))

(qwerty/def scanner
  (qwerty/fn* (rdr)
    (qwerty/let* ((s (qwerty/new scanner.Scanner))
                  (rdr (qwerty/cast io.Reader rdr)))
      (qwerty/do
       (qwerty/go-method-call s Init rdr)
       (qwerty/set! (qwerty/.- s Mode)
                    (qwerty/goref scanner.ScanIdents))
       s))))

(qwerty/def scan
  (qwerty/fn* (sc)
    (qwerty/let* ((sc (qwerty/cast (* scanner.Scanner) sc)))
      (qwerty/go-method-call sc Scan))))

(qwerty/def next-token
  (qwerty/fn* (sc)
    (qwerty/let* ((sc (qwerty/cast (* scanner.Scanner) sc)))
      (qwerty/go-method-call sc TokenText))))

(qwerty/def next
  (qwerty/fn* (sc)
    (qwerty/let* ((sc (qwerty/cast (* scanner.Scanner) sc)))
      (qwerty/go-method-call sc Next))))
