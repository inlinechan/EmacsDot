(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)

;; https://github.com/magit/magit/issues/1743
;; use 2way ediff like we used to do in version 1.x
(setq magit-ediff-dwim-show-on-hunks t)

(eval-after-load 'magit
  (when (and (not window-system) (string-match "1\\.4*" (magit-version)))
    (set-face-background 'magit-log-head-label-tags "Grey85")))
