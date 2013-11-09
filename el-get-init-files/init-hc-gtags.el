(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)
(global-set-key (kbd "C-c C-f") 'gtags-find-file)
(global-set-key (kbd "C-c g f") 'gtags-find-file)
(global-set-key (kbd "C-c g t") 'gtags-find-tag-from-here)
(global-set-key (kbd "C-c g p") 'gtags-find-pattern)
(global-set-key (kbd "C-c g r") 'gtags-find-rtag)
(global-set-key (kbd "C-c g l") 'gtags-find-symbol)

;; TODO
;; (defun global-update-incrementally () (shell-command "global -u -q")) ;; "*Messages*" "*Messages*") )

;; ;; Call gtags update when idle for some time
;; (defcustom my-gtags-update-idle-time 300
;;   "Number of idle seconds before an incremental gtags update is launched"
;;   :group 'my-group
;;   :type 'integer
;;   )

;; ;; http://root42.blogspot.kr/2012/10/improved-handling-of-background-gnu.html
;; ;; initially allow gtags updates
;; (setq my-gtags-update-active t)

;; (run-with-idle-timer my-gtags-update-idle-time t
;;           (lambda ()
;;             (if (and my-gtags-update-active
;;              (not (minibufferp) )
;;              )
;;             (progn
;;               (message "Running gtags...")
;;               (global-update-incrementally)
;;               )
;;           )
;;             )
;;           )

;; (add-hook 'ediff-quit-hook
;;    (lambda ()
;;      (message "Activating gtags update.")
;;      (setq my-gtags-update-active t)
;;      )
;;    )

;; (add-hook 'ediff-before-setup-hook
;;    (lambda ()
;;      (message "Deactivating gtags update.")
;;      (setq my-gtags-update-active nil)
;;      )
;;    )
