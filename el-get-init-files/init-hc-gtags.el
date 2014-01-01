(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)
(global-set-key (kbd "C-c C-f") 'gtags-find-file)
(global-set-key (kbd "C-c g f") 'gtags-find-file)
(global-set-key (kbd "C-c g t") 'gtags-find-tag-from-here)
(global-set-key (kbd "C-c g p") 'gtags-find-pattern)
(global-set-key (kbd "C-c g r") 'gtags-find-rtag)
(global-set-key (kbd "C-c g l") 'gtags-find-symbol)

(add-hook 'gtags-select-mode-hook
  '(lambda ()
     (setq hl-line-face 'underline)
     (hl-line-mode 1)))

;; Update GNU global upon save
;; http://www.emacswiki.org/emacs/GnuGlobal#toc3
(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  ;; (call-process "global" nil nil nil "-u")
  (start-process "update-gtags" nil "global" "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(dolist (mode-hook (list
               'c++-mode-hook
               'c-mode-hook))
  (add-to-list 'after-save-hook #'gtags-update-hook))

(setq gtags-path-style 'relative)

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
