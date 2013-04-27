(qwerty/package qwerty)
;; (qwerty/import io)
(qwerty/import bufio)

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

(qwerty/def r/unread!
  (qwerty/fn* (rdr)
    (qwerty/let* ((rdr (qwerty/cast *bufio.Reader rdr)))
      (qwerty/go-method-call rdr UnreadRune))))


;; (qwerty/def lisp/read
;;   (qwerty/fn* (rdr)
;;     (qwerty/let* ((result nil))
;;       (qwerty/do
;;        (qwerty/labels
;;         start
;;         (qwerty/let* ((r (r/one-rune rdr)))
;;           (qwerty/if (qwerty/= r \()
;;             (lisp/read-list rdr)
;;             (qwerty/if (qwerty/= r \space)
;;               (qwerty/do
;;                (qwerty/goto start)
;;                nil)
;;               (qwerty/if (qwerty/= r \newline)
;;                 (qwerty/do
;;                  (qwerty/goto start)
;;                  nil)
;;                 (qwerty/if (qwerty/= r \tab)
;;                   (qwerty/do
;;                    (qwerty/goto start)
;;                    nil)
;;                   (qwerty/if (qwerty/= r \,)
;;                     (qwerty/do
;;                      (qwerty/goto start)
;;                      nil)
;;                     (qwerty/do
;;                      (r/unread! rdr)
;;                      (qwerty/set! result (lisp/read-atom rdr))))))))))
;;        result))))

;; (qwerty/def lisp/read-list
;;   (qwerty/fn* (rdr)
;;     (qwerty/let* ((lst nil)
;;                   (rune (r/one-rune rdr)))
;;       (qwerty/if (qwerty/= rune \))
;;         (lisp/reverse lst)))))


;; (qwerty/def lisp/read-atom
;;   (qwerty/fn* (rdr)
;;     (qwerty/let* ((lst nil)
;;                   (rune (r/one-rune rdr)))
;;       (qwerty/if (qwerty/= rune \))
;;         (lisp/reverse lst)))))
