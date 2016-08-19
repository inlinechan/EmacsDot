(add-hook 'js2-mode-hook
          (lambda ()
            (when (not (tramp-tramp-file-p (buffer-file-name)))
              (tern-mode t))))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (setq tern-ac-on-dot t)
     (tern-ac-setup)))
