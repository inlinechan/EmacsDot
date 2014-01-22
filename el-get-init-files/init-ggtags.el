;; https://github.com/leoliu/ggtags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)
              )))

;; Unmap M-]
;; http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
(defun my-ggtags-mode-hook()
  (let ((oldmap (cdr (assoc 'ggtags-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (message "%s" "init-ggtags")
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "M-]") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(ggtags-mode . ,newmap) minor-mode-overriding-map-alist))
)

(add-hook 'ggtags-mode-hook 'my-ggtags-mode-hook)
