(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)

(eval-after-load 'magit
  '(progn
     (when (not window-system)
       (set-face-background 'magit-log-head-label-tags "Grey85"))))
