;; mark could be noticable
(setq-default transient-mark-mode t)

;; no backup ( start with ~(tilt) )
(setq-default make-backup-files nil)

;; column limit 80
(setq fill-column 80)

;; text-mode is default
(setq default-major-mode 'text-mode)

;; parenthesis matching
;; http://www.emacswiki.org/cgi-bin/wiki/parenthesismatching
(defun goto-match-paren (arg)
  "go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; purpose: when you visit a file, point goes to the last place where
;; it was when you previously visited the same file.
;;
;; http://www.emacswiki.org/cgi-bin/wiki/saveplace
(require 'saveplace)
(setq-default save-place t)

;; ediff
;; http://www.emacswiki.org/emacs/EdiffMode
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))
;; (setq ediff-split-window-function 'split-window-horizontally)

;;    If `gdb-many-windows' is non-`nil', then `M-x gdb' displays the
;; following frame layout:

;;      +--------------------------------+--------------------------------+
;;      |   GUD buffer (I/O of GDB)      |   Locals/Registers buffer      |
;;      |--------------------------------+--------------------------------+
;;      |   Primary Source buffer        |   I/O buffer for debugged pgm  |
;;      |--------------------------------+--------------------------------+
;;      |   Stack buffer                 |   Breakpoints/Threads buffer   |
;;      +--------------------------------+--------------------------------+
(setq gdb-many-windows t)

;; dircmp-mode
;; (load "~/.emacs.d/dircmp.el")

;; ediff marked file
(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
  
  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files)) 
            (buffer-file-name (nth 2 marked-files)))))

;; getting git root dir
;; http://blog.uberweiss.net/2009/11/scoping-emacs-to-a-git-root-directory.html
(defun my-git-root ()
  (if buffer-file-name
      (let* ((current-directory (file-name-directory buffer-file-name))
             (git-directory (concat current-directory ".git")))
        (while (and
                current-directory
                (not (file-exists-p git-directory)))
          (setq current-directory (file-name-directory (substring current-directory 0 -1)))
          (setq git-directory (concat current-directory ".git")))
        current-directory)))

;; Go to the line of the file easily especially in gdb call stack.
;; /etc/passwd:10
(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))

(global-set-key (kbd "C-<return>") 'find-file-at-point-with-line)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
;; (global-auto-revert-mode 1)

;; http://www.emacswiki.org/emacs/SearchAtPoint
;; http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
;; I-search with initial contents
(defvar isearch-initial-string nil)
(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))
(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun other-window-prev (&optional step)
  "other-window to opposite direction"
  (interactive "P")
  (setq step -1)
  (other-window step))


;; recentf
(require 'recentf)

;; enable recent files mode.
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notab

;; notab by default
(setq-default indent-tabs-mode nil)

(defun hc/notab ()
  "Use 4 spaces instead of tab and also use spaces for indentation"
  (setq default-tab-width 4)
  (setq c-basic-offset 4)               ;; indent use only 4 blanks
  (setq indent-tabs-mode nil)           ;; no tab
  )

(dolist (mode (list
               'c++-mode-hook
               'c-mode-hook
               'cperl-mode-hook
               'css-mode-hook
               'emacs-lisp-mode-hook
               'git-commit-mode-hook
               'java-mode-hook
               'js2-mode-hook
               'perl-mode-hook
               'python-mode-hook
               'sh-mode-hook
               ))
  (add-hook mode 'hc/notab))

;; c/c++ mode
(defun hc/c-style ()
  "C/C++ style"
  (c-set-style "bsd")
  (setq c-basic-offset 4))

(defun switch-header-impl()
  "Switch between header(.h) and impl(.c or .cpp)"
  (local-set-key (kbd "M-p") 'ff-find-other-file))

(dolist (mode (list
               'c++-mode-hook
               'c-mode-hook))
  ;; (add-hook mode 'hc/c-style)
  (add-hook mode 'switch-header-impl))

;; WebKit hook
;; https://github.com/omo/dotfiles/blob/master/.emacs#L178
(defun webkit-c-mode-hook ()
  (interactive)
  (setq c-basic-offset 4)
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'substatement-open 0))

(defun hc/c-mode-hook ()
  (if (or (string-match "WebCore" buffer-file-name)
          (string-match "JavaScriptCore" buffer-file-name)
          (string-match "WebKit" buffer-file-name)
          (string-match "WebKit2" buffer-file-name))
      (webkit-c-mode-hook)
    (if (string-match "chromium" buffer-file-name)
        (google-set-c-style)
      (hc/c-style))))

(add-hook 'c-mode-common-hook 'hc/c-mode-hook)

;; ;; tab width
;; (setq default-tab-width 4)
;; (setq c-basic-offset 4)                 ;; indent use only 4 spaces
;; (setq-default indent-tabs-mode nil)     ;; no tab

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hook
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; css-mode
(autoload 'css-mode "css-mode-simple")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; makefile-mode
(setq auto-mode-alist
      (append
       '(
         ;; makefile
         ("makefile\\."  . makefile-mode)
         ("Makefile\\.*" . makefile-mode)
         ("\\.mak$"      . makefile-mode)
         ("\\.min$"      . makefile-mode)
         ("Android.mk$"  . makefile-mode)
         ;;  perl
         ("\\.\\([pp][llm]\\|al\\)\\'" . cperl-mode)
         )
       auto-mode-alist))

(defun hc/decide-c-c++-mode ()
  "Detect .h file as c-mode in some case"
  (setq cfilename (concat (file-name-sans-extension buffer-file-name) ".c"))
  (if (or (file-exists-p cfilename)
          (string-match "cairo" buffer-file-name))
      (c-mode)
    (c++-mode)))

(add-hook 'c-mode-hook 'hc/decide-c-c++-mode)

;; perl mode
(setq interpreter-mode-alist
      (append
       '(("perl"     . cperl-mode)
         ("perl5"    . cperl-mode)
         ("miniperl" . cperl-mode)) interpreter-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-function-mode
;; http://www.emacswiki.org/emacs/WhichFuncMode#WhichFunctionMode
(which-function-mode)

(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))

(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-format (delete (assoc 'which-func-mode
                                          mode-line-format) mode-line-format)
          header-line-format which-func-header-line-format)))

(dolist (mode (list
               'c++-mode
               'c-mode
               'java-mode
               'js2-mode
               'lisp-mode
               'python-mode
               'sh-mode))
  (add-to-list 'which-func-modes mode))

(dolist (mode (list
               ;; 'c++-mode-hook      ;; wrong detection with starting || statement
               ;; 'c-mode-hook
               'cperl-mode-hook
               'css-mode-hook
               'emacs-lisp-mode-hook
               'git-commit-mode-hook
               'java-mode-hook
               'js2-mode-hook
               'perl-mode-hook
               'python-mode-hook
               'sh-mode-hook
               ))
  (add-hook mode 'turn-on-orgtbl))

(provide 'hc-general)
