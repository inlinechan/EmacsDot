(eval-after-load 'flycheck
  `(progn
     (flycheck-add-mode 'json-jsonlint 'json-mode)
     (add-hook 'json-mode-hook 'flycheck-mode)))
