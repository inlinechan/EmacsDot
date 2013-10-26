(require 'js2-mode)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

;; TODO
;; (defun my-js2-mode-hook ()
;;   (jquery-doc-setup)
;;   (local-set-key (kbd "C-c .") 'ac-complete-jquery))

;; (add-hook 'js2-mode-hook 'my-js2-mode-hook)
