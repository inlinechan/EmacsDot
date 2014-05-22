(require 'js2-mode)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode))
              auto-mode-alist))

(defun override-gtags-find-file-hook ()
  (local-set-key (kbd "C-c C-f") 'gtags-find-file)
  (local-set-key (kbd "<f5>") 'js2-mode-toggle-hide-functions))

(add-hook 'js2-mode-hook 'override-gtags-find-file-hook)

;; TODO
;; (defun my-js2-mode-hook ()
;;   (jquery-doc-setup)
;;   (local-set-key (kbd "C-c .") 'ac-complete-jquery))

;; (add-hook 'js2-mode-hook 'my-js2-mode-hook)
