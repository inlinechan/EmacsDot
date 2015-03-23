(add-hook 'rtags-mode-hook (lambda ()
                             (setq line-move-visual nil)
                             (define-key rtags-mode-map "n" 'next-line)
                             (define-key rtags-mode-map "p" 'previous-line)
                             (define-key rtags-mode-map "M-n" 'next-line)
                             (define-key rtags-mode-map "M-p" 'previous-line)))

(add-hook 'c++-mode-hook (lambda ()
                           (rtags-enable-standard-keybindings c-mode-base-map)))
