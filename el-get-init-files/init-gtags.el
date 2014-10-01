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

(add-hook 'gtags-mode-hook
          '(lambda ()
             (define-key gtags-mode-map "\e*" 'gtags-pop-stack)
             (define-key gtags-mode-map "\e." 'gtags-find-tag)
             (define-key gtags-mode-map "\C-x4." 'gtags-find-tag-other-window)))

(setq gtags-path-style 'relative)
;; (setq gtags-auto-update t)
