(add-hook 'c++-mode-hook
          (lambda ()
            (when (fboundp 'clang-format)
              (define-key c++-mode-map (kbd "C-M-\\") 'clang-format))))
