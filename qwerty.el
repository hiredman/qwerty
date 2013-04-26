(define-derived-mode qwerty-mode lisp-mode "qwerty script"
  (lisp-mode-variables nil)
  (put 'qwerty/defgofun 'lisp-indent-function 'defun)
  (put 'qwerty/fn* 'lisp-indent-function 1)
  (put 'qwerty/let* 'lisp-indent-function 1)
  (put 'qwerty/godef 'lisp-indent-function 'defun)
  (put 'qwerty/def 'lisp-indent-function 'defun)
  (put 'qwerty/if 'lisp-indent-function 1)
  (put 'qwerty/results 'lisp-indent-function 'defun)
  (put 'qwerty/defgomethod 'lisp-indent-function 'defun)
  (put 'qwerty/definterface 'lisp-indent-function 'defun)
  (font-lock-add-keywords 'qwerty-mode '(("qwerty/*" . font-lock-keyword-face))))

(add-to-list 'auto-mode-alist '("\\.q\\'" . qwerty-mode))

(provide 'qwerty-mode)
