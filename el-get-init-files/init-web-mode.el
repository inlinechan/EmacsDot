(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      s-indent-level 2)

;; Enable this if .js highlight like .jsx
;; (setq web-mode-content-types-alist
;;   '(("jsx" . "\\.js[x]?\\'")))
