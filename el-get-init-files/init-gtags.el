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

(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (define-key gtags-select-mode-map "k" 'previous-line)
             (define-key gtags-select-mode-map "j" 'next-line)
             (define-key gtags-select-mode-map "p" 'previous-line)
             (define-key gtags-select-mode-map "n" 'next-line)
             (define-key gtags-select-mode-map "\ep" 'previous-line)
             (define-key gtags-select-mode-map "\en" 'next-line)
             (define-key gtags-select-mode-map "\e*" 'gtags-pop-stack)
             (define-key gtags-select-mode-map "q" 'gtags-pop-stack)
             (define-key gtags-select-mode-map "u" 'gtags-pop-stack)
             (define-key gtags-select-mode-map "\C-o" 'gtags-select-tag-other-window)
             (define-key gtags-select-mode-map "\eo" 'gtags-select-tag-other-window-focus)
             (define-key gtags-select-mode-map "\e." 'gtags-select-tag)))

(defun tag-from-here ()
  (interactive)
  (if (gtags-get-rootpath)
      (if current-prefix-arg
          (gtags-find-tag)
        (gtags-find-tag-from-here))
    (message "No gtags index found")))

(defun find-rtag ()
  (interactive)
  (if (gtags-get-rootpath)
      (gtags-find-rtag)
    (message "No gtags index found")))

(add-hook 'gtags-mode-hook
          '(lambda ()
             (define-key gtags-mode-map "\e*" 'gtags-pop-stack)
             (define-key gtags-mode-map "\e." 'tag-from-here)
             (define-key gtags-mode-map "\e," 'find-rtag)
             (define-key gtags-mode-map "\C-x4." 'gtags-find-tag-other-window)))

(setq gtags-path-style 'relative)
;; (setq gtags-auto-update t)

(defun enable-gtags-mode ()
  (gtags-mode 1))

(add-hook 'c-mode-hook 'enable-gtags-mode)
(add-hook 'c++-mode-hook 'enable-gtags-mode)

(defun hc-mktag (dir)
  (interactive "Dmktag (directory): ")
  (let ((mktag-script "mktag")
        (buffer-name "*mktag*"))
    (and (executable-find mktag-script)
         (or (cd dir) (error "Fail to change directory to %s" dir))
         (start-process-shell-command mktag-script buffer-name mktag-script)
         (switch-to-buffer buffer-name))))

(defun hc-gtags-update (dir)
  (interactive "Dglobal -u (directory): ")
  (let ((global-script "global")
        (buffer-name "*mktag*"))
    (and (executable-find global-script)
         (or (cd dir) (error "Fail to change directory to %s" dir))
         (let ((result
                (benchmark-run 1
                  (call-process global-script nil buffer-name t "-u"))))
           (message "Took %3.0f ms in running `%s -u in %s'"
                    (* 1000.0 (car result)) global-script dir)))))

(defun hc-gtags-impl-p (name)
  (string-match "\\(cc\\|cpp\\)$" name))

(defun hc-gtags-header-p (name)
  (string-match "\\(h\\|hpp\\)$" name))

(defun hc-gtags-get-counter (name)
  (cond ((hc-gtags-impl-p name)
         (concat (file-name-directory name) (file-name-base name) ".h"))
        ((hc-gtags-header-p name)
         (concat (file-name-directory name) (file-name-base name) ".cpp"))
        (t nil)))

(defun hc-gtags-get-counter-candidates (name)
  (with-temp-buffer
    ;; (call-process gtags-global-command nil t nil "-P" (file-name-base name))
    (call-process gtags-global-command nil t nil "-P" (file-name-nondirectory (hc-gtags-get-counter name)))
    (split-string (buffer-string) "\n" t)))

(defun hc-gtags-get-next-match (name)
  (interactive)
  (and (gtags-get-rootpath) (hc-gtags-get-counter-candidates name)))

(defun hc-gtags-switch-cc-to-h ()
  (interactive)
  (let ((basic (hc-get-next-match (buffer-file-name)))
        (gtags (hc-gtags-get-next-match (buffer-file-name))))
    (when basic
      (find-file basic))
    (when gtags
      (if (= 1 (length gtags))
          (find-file (car gtags))
        (gtags-goto-tag (concat "/" (file-name-nondirectory (car gtags))) "P"))
      )))

(defun hc-gtags-switch-header-impl()
  (local-unset-key (kbd "C-M-p"))
  (local-set-key (kbd "C-M-p") 'hc-gtags-switch-cc-to-h))

(dolist (mode (list
               'c++-mode-hook
               'c-mode-hook))
  (add-hook mode 'hc-gtags-switch-header-impl))

(global-set-key (kbd "C-c t") 'hc-mktag)
(global-set-key (kbd "C-c u") 'hc-gtags-update)

(setq gtags-ignore-case nil)
