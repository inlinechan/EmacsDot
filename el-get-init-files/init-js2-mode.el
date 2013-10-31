(require 'js2-mode)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode))
              auto-mode-alist))

;; TODO
;; (defun my-js2-mode-hook ()
;;   (jquery-doc-setup)
;;   (local-set-key (kbd "C-c .") 'ac-complete-jquery))

;; (add-hook 'js2-mode-hook 'my-js2-mode-hook)
