(require 'auto-complete-clang)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

(add-hook 'c++-mode-hook (lambda ()
               (add-to-list 'ac-sources 'ac-source-clang)))
(add-hook 'c++-mode-hook
      (lambda ()
        (local-set-key (kbd "C-c .") 'ac-complete-clang)))

(defun include-path-list-for-clang-from-g++ ()
  "Return include path for auto-complete-clang from g++"
  (let ((command "echo \"\" | g++ -v -x c++ -E -")
        (tmp-buffer-name "*temp*")
        (include-path)
        (start-pos)
        (end-pos))
    (when (bufferp tmp-buffer-name)
      (kill-buffer tmp-buffer-name))
    (call-process-shell-command command  nil tmp-buffer-name)
    (save-excursion
      (set-buffer tmp-buffer-name)
      (beginning-of-buffer)
      (re-search-forward "^#include\s_*<[^:]+:$")
      (setq start-pos (point))
      (re-search-forward "^End of search list.$")
      (move-beginning-of-line 1)
      (setq end-pos (point))
      (setq include-path-string (buffer-substring start-pos end-pos))
      (kill-buffer tmp-buffer-name)
      (when include-path-string
        (mapcar (lambda (item)(concat "-I" item))
                (split-string include-path-string))))))

(setq ac-clang-flags (include-path-list-for-clang-from-g++))
(add-to-list 'ac-clang-flags "-std=c++11")
