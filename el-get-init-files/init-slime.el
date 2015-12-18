;; http://www.cliki.net/slime%20tips
(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'cliki:start-slime)
;; (setq inferior-lisp-program "clisp -K full")

(add-hook 'lisp-mode-hook
          (defun my-lisp-mode-hook ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))

(when (functionp 'slime-setup-command-hooks)
  (slime-setup-command-hooks))

(slime-setup '(slime-autodoc))
