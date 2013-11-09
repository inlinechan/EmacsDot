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

(add-to-list 'flycheck-checkers 'hc/webkit-style)
(add-hook 'c++-mode-hook 'flycheck-mode)

(global-set-key (kbd "C-<f8>") 'flycheck-mode)