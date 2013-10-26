;;(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-c r y") 'comment-region)
(global-set-key (kbd "C-c r u") 'uncomment-region)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-c m") 'manual-entry)    ;; manpage
(global-set-key (kbd "M-]") 'goto-match-paren)  ;; goto matching parenthesis

;; find from current dir
(global-set-key (kbd "C-c C-g") 'find-name-dired)
;; ask dir to find before
(global-set-key (kbd "C-c C-h") 'find-grep-dired)
(global-set-key (kbd "C-c g g") 'grep-find)

(global-set-key (kbd "C-c C-k") 'isearch-forward-at-point)
(global-set-key (kbd "C-M-o") 'other-window)

(global-set-key (kbd "C-M-o") 'other-window)
(global-set-key (kbd "C-M-m") 'other-window-prev)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(provide 'hc-general-key)
