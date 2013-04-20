;; (defvar mydsl-events
;;   '("reservedword1"
;;     "reservedword2"))

;; (defvar mydsl-keywords
;;   '("other-keyword" "another-keyword"))

;; ;; I'd probably put in a default that you want, as opposed to nil
;; (defvar mydsl-tab-width nil "Width of a tab for MYDSL mode")

;; ;; Two small edits.
;; ;; First is to put an extra set of parens () around the list
;; ;; which is the format that font-lock-defaults wants
;; ;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; ;; you were very close
;; (defvar mydsl-font-lock-defaults
;;   `((
;;      ;; stuff between "
;;      ("\"\\.\\*\\?" . font-lock-string-face)
;;      ;; ; : , ; { } =>  @ $ = are all special elements
;;      (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
;;      ( ,(regexp-opt mydsl-keywords 'words) . font-lock-builtin-face)
;;      ( ,(regexp-opt mydsl-events 'words) . font-lock-constant-face)
;;      )))

(define-derived-mode qwerty-mode clojure-mode "qwerty script"
  "qwerty mode is a mode for editing qwert lisp"
  (lisp-mode-variables nil)
  ;; (set (make-local-variable 'lisp-indent-function)
  ;;      'clojure-indent-function)
  ;; (setq font-lock-defaults mydsl-font-lock-defaults)

  ;; when there's an override, use it
  ;; otherwise it gets the default value
  ;; (when mydsl-tab-width
  ;;   (setq tab-width mydsl-tab-width))

  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  ;; (setq comment-start "#")
  ;; (setq comment-end "")

  ;; (modify-syntax-entry ?# "< b" mydsl-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" mydsl-mode-syntax-table)
  ;; ;;A gnu-correct program will have some sort of hook call here.
  )

(provide 'qwerty-mode)
