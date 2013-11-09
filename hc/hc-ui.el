;; no splash
(setq inhibit-startup-message t)

;; hide toolbar & menubar
;;    (tool-bar-mode -1)
;;    (menu-bar-mode -1)

;; color theme
;; (setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

(if (require 'color-theme nil 'noerror)
    (progn
      (color-theme-initialize)
      (color-theme-clarity)))

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
               (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(add-to-list 'default-frame-alist
         '(font . "DejaVu Sans Mono-10"))

(provide 'hc-ui)
