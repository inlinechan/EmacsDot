(require 'highlight-chars)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

(dolist (mode (list
               'c++-mode-hook
               'c-mode-hook
               'diff-mode-hook
               'emacs-lisp-mode-hook
               'git-commit-mode-hook
               'js2-mode-hook
               'python-mode-hook
               ))
  (add-hook mode 'hc-highlight-trailing-whitespace))

(global-set-key (kbd "<f8>") 'hc-toggle-highlight-trailing-whitespace)
