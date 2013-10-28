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

;; blank-mode
;; (require 'blank-mode)
(eval-after-load 'blank-mode
  (global-set-key (kbd "C-c b") 'blank-mode))

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
(defun notab ()
  "use 4 spaces instead of tab and also use spaces for indentation"
  (setq default-tab-width 4)
  (setq c-basic-offset 4)               ;; indent use only 4 blanks
  (setq indent-tabs-mode nil)           ;; no tab
  )  

(add-hook 'c-mode-hook 'notab)
(add-hook 'c-mode-hook '
	  (lambda () (c-set-style "bsd")))
(add-hook 'c++-mode-hook 'notab)
(add-hook 'c++-mode-hook '
	  (lambda () (c-set-style "bsd")))

(add-hook 'jave-mode-hook 'notab)
(add-hook 'css-mode-hook 'notab)
(add-hook 'python-mode-hook 'notab)
(add-hook 'perl-mode-hook 'notab)
(add-hook 'cperl-mode-hook 'notab)
(add-hook 'emacs-lisp-mode-hook 'notab)

;; tab width
(setq default-tab-width 4)
(setq c-basic-offset 4)                 ;; indent use only 4 spaces
(setq-default indent-tabs-mode nil)     ;; no tab

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hook
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; css-mode
(autoload 'css-mode "css-mode-simple")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; js2 mode
(if (require 'js2-mode nil 'noerror)
    (progn
      ;; (add-to-list 'load-path "~/.emacs.d/js2-mode")
      ;; (autoload 'js2-mode "js2-mode" nil t)
      (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  (message "js2-mode initialized"))

;; makefile
(setq auto-mode-alist
      (append
       '(("makefile\\." . makefile-mode)
	 ("Makefile\\.*" . makefile-mode)
	 ("\\.mak"      . makefile-mode)
	 ;; ("\\.pri" . makefile-mode)
	 ;; ("\\.pro" . makefile-mode)
	 ("\\.prf" . makefile-mode)
	 ("\\.min" . makefile-mode)
	 ("Android.mk" . makefile-mode))
       auto-mode-alist))

;; perl mode
(add-to-list 'auto-mode-alist '("\\.\\([pp][llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; c++-mode
(setq auto-mode-alist
      (append
       '(("\\.h$"      . c++-mode))
       auto-mode-alist))

(defun switch-header-impl()
  "Switch between header(.h) and impl(.c or .cpp)"
  (local-set-key (kbd "M-p") 'ff-find-other-file))

(add-hook 'c++-mode-hook 'switch-header-impl)
(add-hook 'c-mode-hook 'switch-header-impl)

(provide 'hc-general)


