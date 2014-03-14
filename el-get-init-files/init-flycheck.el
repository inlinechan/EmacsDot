;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'flycheck)

(setq verbose t)

(defun hc/get-webkit-dir ()
  "Return WebKit dir under $HOME."
  (let ((home-dir (getenv "HOME")))
    (dolist (dir (list "QtWebKit" "WebKit" "src/WebKit"))
      (setq full-path (concat home-dir "/" dir))
      (when (file-exists-p full-path)
        (return full-path)))))

(defun hc/get-check-script-path ()
  "Return check-webkit-style script."
  (let ((webkit-dir (hc/get-webkit-dir)))
    (when webkit-dir
      (concat webkit-dir "/" "Tools/Scripts/check-webkit-style"))))

(defun hc/register-webkit-style-checker (script-path)
  (when script-path
    (flycheck-define-checker hc/webkit-style
      "A syntax checker for WebKit.

See URL `http://www.webkit.org/coding/coding-style.html'."
      :command ("python"
                (eval (hc/get-check-script-path))
                source)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" (message) line-end))
      :modes (c++-mode)
      :next-checkers ((warnings-only . c/c++-cppcheck))
      :predicate
      (lambda ()
        (when verbose
          (message "#### hc/webkit-style predicate %s" buffer-file-name))
        (and buffer-file-name
             (hc/get-check-script-path)
             (or (string-match "WebCore" buffer-file-name)
                 (string-match "JavaScriptCore" buffer-file-name)
                 (string-match "WebKit" buffer-file-name)
                 (string-match "WebKit2" buffer-file-name)))))
    (add-to-list 'flycheck-checkers 'hc/webkit-style)))


(defun hc/flycheck-add-mode-hook ()
  "Enable flycheck in some modes"
  (interactive)
  (mapcar #'(lambda (mode) (add-hook mode 'flycheck-mode))
          '(c++-mode-hook
            sh-mode-hook
            emacs-lisp-mode-hook
            html-mode-hook
            python-mode-hook)))

(defun hc/flycheck-remove-mode-hook ()
  "Disable flycheck in some modes"
  (interactive)
  (mapcar #'(lambda (mode) (remove-hook mode 'flycheck-mode))
          '(c++-mode-hook
            sh-mode-hook
            emacs-lisp-mode-hook
            html-mode-hook
            python-mode-hook)))

(defun hc/toggle-flycheck ()
  "Toggle flycheck mode."
  (interactive)
  (when verbose
    (message "%s %s" "flycheck-mode" flycheck-mode))
  (let ((old-mode flycheck-mode))
    (if old-mode
        (hc/flycheck-remove-mode-hook)
      (hc/flycheck-add-mode-hook))
    (when flycheck-mode
      (setq current-prefix-arg nil))
    (call-interactively 'flycheck-mode)))

(hc/register-webkit-style-checker (hc/get-check-script-path))
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(hc/flycheck-add-mode-hook)

(global-set-key (kbd "C-<f8>") 'hc/toggle-flycheck)

;;; init-flycheck ends here
