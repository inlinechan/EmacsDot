;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'flycheck)

(defun hc/get-webkit-dir ()
  "Return WebKit dir under $HOME."
  (setq home-dir (getenv "HOME"))
  (dolist (dir (list "QtWebKit" "WebKit" "src/WebKit"))
    (setq full-path (concat home-dir "/" dir))
    (when (file-exists-p full-path)
      (return full-path))))

(defun hc/get-check-script-path ()
  "Return check-webkit-style script."
  (concat (hc/get-webkit-dir) "/" "Tools/Scripts/check-webkit-style"))

(setq check-webkit-style-path (hc/get-check-script-path))

(flycheck-define-checker hc/webkit-style
  "A syntax checker for WebKit.

See URL `http://www.webkit.org/coding/coding-style.html'."
  :command ("python"
            (eval check-webkit-style-path)
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" (message) line-end))
  :modes (c++-mode)
  :next-checkers ((warnings-only . c/c++-cppcheck)))

(defun hc/flycheck-c++-mode-select-checker ()
  "Select check based on buffer name"
  (if (or (string-match "WebCore" buffer-file-name)
          (string-match "JavaScriptCore" buffer-file-name)
          (string-match "WebKit" buffer-file-name)
          (string-match "WebKit2" buffer-file-name))
      (flycheck-select-checker 'hc/webkit-style)
    (when (executable-find "clang++")
      (flycheck-select-checker 'c/c++-clang))))

(defun hc/add-flycheck-mode-hook ()
  "Enable flycheck-mode if necessary."
  (interactive)
  ;; Enable check-webkit-style only for WebKit source code
  (when (eq major-mode 'c++-mode)
    (add-hook 'c++-mode-hook 'hc/flycheck-c++-mode-select-checker))
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'html-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode))

(defun hc/flycheck-add-default-mode-hook ()
  "Add default mode for flycheck."
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'html-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode))

(defun hc/remove-flycheck-mode-hook ()
  "Remove flycheck mode hook."
  (interactive)
  ;; Disable flycheck mode check-webkit-style in WebKit buffer
  (when (eq major-mode 'c++-mode)
    (remove-hook 'c++-mode-hook 'hc/flycheck-c++-mode-select-checker))
  (remove-hook 'sh-mode-hook 'flycheck-mode)
  (remove-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (remove-hook 'html-mode-hook 'flycheck-mode)
  (remove-hook 'python-mode-hook 'flycheck-mode))

(defun hc/toggle-flycheck ()
  "Toggle flycheck mode."
  (interactive)
  ;; (message "%s %s" "flycheck-mode" flycheck-mode)
  (let ((old-mode flycheck-mode))
    (if old-mode
        (hc/remove-flycheck-mode-hook)
      (hc/add-flycheck-mode-hook))
    (when flycheck-mode
      (setq current-prefix-arg nil))
    (call-interactively 'flycheck-mode)))

(add-to-list 'flycheck-checkers 'hc/webkit-style)
(hc/flycheck-add-default-mode-hook)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "C-<f8>") 'hc/toggle-flycheck)

;;; init-flycheck ends here
