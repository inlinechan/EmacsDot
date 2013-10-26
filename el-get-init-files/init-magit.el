(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)
;; (if (require 'magit nil 'noerror)
;;     ;; (eval-after-load 'magit
;;     (progn
;;       (require 'magit)
;;       (global-set-key (kbd "C-c s") 'magit-status)))

;; http://readystate4.com/2011/02/22/emacs-changing-magits-default-diff-colors/
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))
