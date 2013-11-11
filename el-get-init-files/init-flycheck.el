(require 'flycheck)

(defun hc/get-webkit-dir ()
  "Return WebKit dir under $HOME"
  (setq home-dir (getenv "HOME"))
  (dolist (dir (list "WebKit" "src/WebKit" "QtWebKit"))
    (setq full-path (concat home-dir "/" dir))
    (when (file-exists-p full-path)
      (return full-path))))

(defun hc/get-check-script-path ()
  "Return check-webkit-style script"
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

(defun hc/enable-check-webkit-style ()
  "Enable check-webkit-style only for WebKit source code"
  (interactive)
  (flycheck-mode)
  (add-hook 'c++-mode-hook 'hc/enable-check-webkit-style)
  (if (or (string-match "WebCore" buffer-file-name)
          (string-match "JavaScriptCore" buffer-file-name)
          (string-match "WebKit" buffer-file-name)
          (string-match "WebKit2" buffer-file-name))
      (flycheck-select-checker 'hc/webkit-style)
    (flycheck-select-checker 'c/c++-clang)))

(add-to-list 'flycheck-checkers 'hc/webkit-style)
;; (add-hook 'c++-mode-hook 'hc/enable-check-webkit-style)

(defun hc/disable-check-webkit-style ()
  "Disable flycheck mode"
  (interactive)
  (remove-hook 'c++-mode-hook 'hc/enable-check-webkit-style))

(defun hc/toggle-flycheck ()
  "Toggle flycheck mode"
  (interactive)
  ;; (message "%s %s" "flycheck-mode" flycheck-mode)
  (let ((old-mode flycheck-mode))
    (when flycheck-mode
      (setq current-prefix-arg nil))
    (call-interactively 'flycheck-mode)
    (if old-mode
        (hc/disable-check-webkit-style)
      (hc/enable-check-webkit-style))))

(global-set-key (kbd "C-<f8>") 'hc/toggle-flycheck)
