(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)

(eval-after-load 'magit
  (when (and (not window-system) (string-match "1\\.4*" (magit-version)))
    (set-face-background 'magit-log-head-label-tags "Grey85")))
