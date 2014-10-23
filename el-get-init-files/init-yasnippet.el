;; (add-to-list 'load-path "~/.emacs.d/el-get/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing
(add-hook 'term-mode-hook
          (lambda() (yas-minor-mode -1)))
