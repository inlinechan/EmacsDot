;; Copyright (C) 2013 Hyungchan Kim

;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; Keywords: lisp
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(require 'cl)                           ; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
       ;; "http://localhost/~hyungchan/el-get/el-get-install.el"
       )
    (let (el-get-master-branch)
    (goto-char (point-max))
    (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

(eval-after-load 'el-get
  '(progn
     (message "eval-after-load el-get")

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; configurations
     (defvar section-autocomplete t)
     (defvar section-automodehook t)
     (defvar section-cedet nil)
     (defvar section-environment t)
     (defvar section-flymake t)
     (defvar section-general t)
     (defvar section-gtags t)
     (defvar section-hotkey t)
     (defvar section-ido t)
     (defvar section-korean t)
     (defvar section-magit t)
     (defvar section-notab t)
     (defvar section-recentf t)
     (defvar section-shell t)
     (defvar section-ui t)
     (defvar section-jedi t)            ;; need python-virtualenv
     (defvar section-qml-mode t)
     (defvar section-gdb-ui t)
     (defvar section-gnuplot-mode t)
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; el-get :: now set our own packages
     (setq
      my:el-get-packages
      '(auto-complete
        auto-complete-clang
        auto-complete-css
        auto-complete-emacs-lisp
        auto-complete-etags
        blank-mode
        color-theme
        flymake-cursor
        graphviz-dot-mode
        jedi
        js2-mode
        jquery-doc
        magit
        php-mode-improved
        popup
        popup-pos-tip
        pos-tip
        psvn
        qmake-mode
        qml-mode
        ))

     ;;
     ;; Some recipes require extra tools to be installed
     ;;
     ;; Note: el-get-install requires git, so we know we have at least that.
     ;;
     ;; (when (el-get-executable-find "cvs")
     ;;   (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

     ;; (when (el-get-executable-find "svn")
     ;;   (loop for p in '(psvn                 ; M-x svn-status
     ;;                    yasnippet            ; powerful snippet mode
     ;;                    )
     ;;         do (add-to-list 'my:el-get-packages p)))

     (setq my:el-get-packages
           (append
            my:el-get-packages
            (loop for src in el-get-sources collect (el-get-source-name src))))

     ;; install new packages and init already installed packages
     (el-get 'sync my:el-get-packages)

     ;; on to the visual settings
     ;; (setq inhibit-splash-screen t)          ; no splash screen, thanks
     ;; (line-number-mode 1)                    ; have line numbers and
     ;; (column-number-mode 1)                  ; column numbers in the mode line

     ;; (tool-bar-mode -1)                      ; no tool bar with icons
     ;; (scroll-bar-mode -1)                    ; no scroll bars
     ;; (unless (string-match "apple-darwin" system-configuration)
     ;;   ;; on mac, there's always a menu bar drown, don't have it empty
     ;;   (menu-bar-mode -1))

     ;; choose your own fonts, in a system dependant way
     ;; (if (string-match "apple-darwin" system-configuration)
     ;;     (set-face-font 'default "Monaco-13")
     ;;   (set-face-font 'default "Monospace-10"))

     ;; (global-hl-line-mode)                        ; highlight current line
     ;; (global-linum-mode 1)                        ; add line numbers on the left

     ;; avoid compiz manager rendering bugs
     (add-to-list 'default-frame-alist '(alpha . 100))

     ;; under mac, have Command as Meta and keep Option for localized input
     (when (string-match "apple-darwin" system-configuration)
       (setq mac-allow-anti-aliasing t)
       (setq mac-command-modifier 'meta)
       (setq mac-option-modifier 'none))


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; section
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (when section-general (message "General...")
           ;; mark could be noticable
           (setq-default transient-mark-mode t)

           ;; no backup ( start with ~(tilt) )
           (setq-default make-backup-files nil)

           ;; column limit 80
           (setq fill-column 80)

           ;; text-mode is default
           (setq default-major-mode 'text-mode)
           (add-hook 'text-mode-hook 'turn-on-auto-fill)

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

           ;; gdict
           ;; (add-to-list 'load-path "~/.emacs.d")
           ;; (require 'gdict)
           ;; (require 'json)
           ;; (global-set-key (kbd "C-c g d") 'gdict)

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

           (message "General... done"))

     (when section-korean (message "Korean...")
           ;; hangul configuration
           ;; (set-language-environment "korean")
           (set-language-environment "UTF-8")
           (setq default-input-method "korean-hangul")
           ;; (global-set-key (kbd "S-SPC") 'toggle-input-method)
           (global-set-key (kbd "<Hangul>") 'toggle-input-method)
           (message "Korean... done"))

     (when section-notab (message "no tab...")
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

           (message "no tab... done"))

     (when section-ui (message "UI customize...")
           ;; no splash
           (setq inhibit-startup-message t)

           ;; hide toolbar & menubar
           ;;    (tool-bar-mode -1)
           ;;    (menu-bar-mode -1)

           ;; color theme
           ;; (setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
           
           (if (require 'color-theme nil 'noerror)
               (progn
                 (color-theme-initialize)
                 (color-theme-clarity)))

           ;; full screen
           (defun fullscreen ()
             (interactive)
             (set-frame-parameter nil 'fullscreen
                                  (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
           (global-set-key [f11] 'fullscreen)

           (add-to-list 'default-frame-alist
                        '(font . "DejaVu Sans Mono-10"))

           (message "UI customize... done"))

     ;; **
     (when section-hotkey (message "hotkey...")
           ;;(global-set-key (kbd "C-c y") 'clipboard-yank)
           (global-set-key (kbd "C-c c") 'compile)
           (global-set-key (kbd "C-c r y") 'comment-region)
           (global-set-key (kbd "C-c r u") 'uncomment-region)
           (global-set-key (kbd "M-g") 'goto-line)

           (global-set-key (kbd "C-c m") 'manual-entry)    ;; manpage
           (global-set-key (kbd "M-]") 'goto-match-paren)  ;; goto matching parenthesis

           ;; find from current dir
           (global-set-key (kbd "C-c C-g") 'find-name-dired)
           ;; ask dir to find before
           (global-set-key (kbd "C-c C-h") 'find-grep-dired)
           (global-set-key (kbd "C-c g g") 'grep-find)

           ;; execute the shell buffer in utf-8 encoding.
           ;; (defun unicode-shell ()
           ;;   "execute the shell buffer in utf-8 encoding.
           ;; note that you'll need to set the environment variable lang and others
           ;; appropriately."
           ;;   (interactive)
           ;;   (let ((coding-system-for-read 'utf-8)
           ;;         (coding-system-for-write 'utf-8)
           ;;         (coding-system-require-warning t))
           ;;     (call-interactively 'shell)))

           (global-set-key (kbd "C-c C-k") 'isearch-forward-at-point)
           (global-set-key (kbd "C-M-o") 'other-window)

           (global-set-key (kbd "C-M-o") 'other-window)
           (global-set-key (kbd "C-M-m") 'other-window-prev)

           (message "hotkey... done"))

     (when section-automodehook (message "automodehook...")
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

           (add-hook 'c++-mode-hook
                     (lambda ()
                       ;; switch h <-> cpp
                       ;; (local-set-key (kbd "M-p") 'eassist-switch-h-cpp)
                       (local-set-key (kbd "M-p") 'ff-find-other-file)))

           (message "automodehook..."))

     (when section-cedet (message "cedet...")
           ;; cedet

           ;; http://www.emacswiki.org/emacs/collectionofemacsdevelopmentenvironmenttools
           (setq byte-compile-warnings nil)

           ;; load cedet.
           ;; see cedet/common/cedet.info for configuration details.
           (load-file "~/.emacs.d/cedet-1.0.1/common/cedet.el")

           ;; enable ede (project management) features
           (global-ede-mode t)

           (semantic-load-enable-excessive-code-helpers)
           (require 'semantic-ia)

           ;; enable ede for a pre-existing c++ project
           ;; (ede-cpp-root-project "name" :file "~/myproject/makefile")


           ;; enabling semantic (code-parsing, smart completion) features
           ;; select one of the following:

           ;; * this enables the database and idle reparse engines
           (semantic-load-enable-minimum-features)

           ;; * this enables some tools useful for coding, such as summary mode
           ;;   imenu support, and the semantic navigator
           (semantic-load-enable-code-helpers)

           ;; * this enables even more coding tools such as intellisense mode
           ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
           (semantic-load-enable-gaudy-code-helpers)

           ;; * this enables the use of exuberent ctags if you have it installed.
           ;;   if you use c++ templates or boost, you should not enable it.
           ;; (semantic-load-enable-all-exuberent-ctags-support)
           ;;   or, use one of these two types of support.
           ;;   add support for new languges only via ctags.
           ;; (semantic-load-enable-primary-exuberent-ctags-support)
           ;;   add support for using ctags as a backup parser.
           ;; (semantic-load-enable-secondary-exuberent-ctags-support)

           (require 'semanticdb)
           (global-semanticdb-minor-mode 1)

           (require 'semanticdb-global)
           (semanticdb-enable-gnu-global-databases 'c-mode)
           (semanticdb-enable-gnu-global-databases 'c++-mode)
           (semanticdb-enable-gnu-global-databases 'java-mode)

           ;; enable srecode (template management) minor-mode.
           ;; (global-srecode-minor-mode 1)

           ;; ecb
           (add-to-list 'load-path "~/.emacs.d/ecb-snap")
           (require 'ecb)
           (require 'ecb-autoloads)

           (custom-set-variables
            ;; custom-set-variables was added by Custom.
            ;; If you edit it by hand, you could mess it up, so be careful.
            ;; Your init file should contain only one such instance.
            ;; If there is more than one, they won't work right.
            '(ecb-options-version "2.40"))
           ;;  '(ecb-options-version "2.40")
           ;;  '(ecb-source-path (quote (("/home/hyungchan/android/external/webkit" "webkit")))))
           ;; ecb window hotkey
           (global-set-key (kbd "M-0") 'ecb-goto-window-edit-last)
           (global-set-key (kbd "M-1") 'ecb-goto-window-directories)
           (global-set-key (kbd "M-2") 'ecb-goto-window-sources)
           (global-set-key (kbd "M-3") 'ecb-goto-window-methods)
           (global-set-key (kbd "M-4") 'ecb-goto-window-history)

           ;; (global-set-key (kbd "C-c C-e") 'ecb-activate)
           ;; (global-set-key (kbd "C-c C-d") 'ecb-deactivate)

           ;; global regexp search
           (global-set-key (kbd "C-c , h") 'semantic-symref-regexp)

           ;; jump well
           (global-set-key (kbd "C-c , a") 'semantic-ia-fast-jump)

           (message "cedet..."))

     (when section-ido (message "ido...")
           (setq ido-enable-flex-matching t)
           (setq ido-everywhere t)
           (ido-mode 1)

           ;; this setting will force Ido to always create a new buffer (in C-x b) if the name does not exist
           (setq ido-create-new-buffer 'always)

           (setq ido-file-extensions-order '(".cpp" ".c" ".h" ".txt"))

           (message "ido... done"))

     (when section-recentf (message "recentf...")
           (require 'recentf)
           ;; get rid of `find-file-read-only' and replace it with something
           ;; more useful.
           (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

           ;; enable recent files mode.
           (recentf-mode t)

      ;;; 50 files ought to be enough.
           (setq recentf-max-saved-items 50)

           (defun ido-recentf-open ()
             "Use `ido-completing-read' to \\[find-file] a recent file"
             (interactive)
             (if (find-file (ido-completing-read "Find recent file: " recentf-list))
                 (message "Opening file...")
               (message "Aborting")))

           (message "recentf... done"))

     (when section-autocomplete (message "autocomplete...")
           (require 'auto-complete-config)
           (ac-config-default)
           (setq ac-auto-start nil)
           (ac-set-trigger-key "TAB")

           (require 'auto-complete-clang)
           ;; c++
           (add-hook 'c++-mode-hook (lambda ()
                                      (add-to-list 'ac-sources 'ac-source-clang)))

           ;; Complete member name by C-c . for C++ mode.
           (add-hook 'c++-mode-hook
                     (lambda ()
                       (local-set-key (kbd "C-c .") 'ac-complete-clang)))

           (setq ac-clang-flags
                 (mapcar (lambda (item)(concat "-I" item))
                         (split-string
                          "
 /usr/include/c++/4.6
 /usr/include/c++/4.6/x86_64-linux-gnu/.
 /usr/include/c++/4.6/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
"
                          )))

           (defun my-ac-cc-mode-setup ()
             (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
           (add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)
           (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

           ;; adds ac-source-jquery to the ac-sources list
           (add-hook 'js2-mode-hook 'jquery-doc-setup)

           (defun my-js2-mode-hook ()
             (jquery-doc-setup)
             (local-set-key (kbd "C-c .") 'ac-complete-jquery))

           (add-hook 'js2-mode-hook 'my-js2-mode-hook)

           (message "autocomplete... done"))

     (when section-shell (message "shell...")
           ;; M-x shell is a nice shell interface to use, let's make it colorful.  If
           ;; you need a terminal emulator rather than just a shell, consider M-x term
           ;; instead.
           (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
           (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

           ;; If you do use M-x term, you will notice there's line mode that acts like
           ;; emacs buffers, and there's the default char mode that will send your
           ;; input char-by-char, so that curses application see each of your key
           ;; strokes.
           ;;
           ;; The default way to toggle between them is C-c C-j and C-c C-k, let's
           ;; better use just one key to do the same.
           (require 'term)
           (define-key term-raw-map  (kbd "C-'") 'term-line-mode)
           (define-key term-mode-map (kbd "C-'") 'term-char-mode)

           ;; ;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
           ;; ;; Well the real default would be C-c C-j C-y C-c C-k.
           ;; (define-key term-raw-map  (kbd "C-y") 'term-paste)

           (message "shell... done")
           )

     (when section-flymake (message "flymake...")
           (if (and (require 'flymake nil 'noerror)
                    (require 'flymake-cursor nil 'noerror))
               (progn
                 (defun flymake-clang-c++-init ()
                   ;; ediff control(at the bottom while emacs running) buffer-file-name is nil
                   (if (not buffer-file-name)
                       (flymake-mode-off)
                     (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                        'flymake-create-temp-inplace))
                            (local-file (file-relative-name
                                         temp-file
                                         (file-name-directory buffer-file-name)))
                            (git-root-dir (my-git-root)))

                       (if (or (string-match "\.index$" local-file)
                               ;; (not (string-match ".cpp$" local-file))
                               (not git-root-dir))
                           (flymake-mode-off)
                         (if (file-exists-p (concat
                                             git-root-dir "Tools/Scripts/check-webkit-style"))
                             (list "bash" (list "flymake-check-webkit-style.sh" git-root-dir local-file))
                           (list "clang++" (list "-fsyntax-only" "-fno-color-diagnostics" local-file)))))))

                 (defun flymake-clang-c++-load ()
                   (interactive)
                   ;; (message "#### %s " buffer-file-name)
                   (unless (eq buffer-file-name nil)
                     ;; (unless (or (eq buffer-file-name nil)
                     ;;             (not (string-match "\.cpp$" buffer-file-name)))
                     (add-to-list 'flymake-allowed-file-name-masks
                                  '("\\.cpp" flymake-clang-c++-init))
                     (add-to-list 'flymake-allowed-file-name-masks
                                  '("\\.cc" flymake-clang-c++-init))
                     (add-to-list 'flymake-allowed-file-name-masks
                                  '("\\.h" flymake-clang-c++-init))
                     (flymake-mode t)
                     (global-set-key (kbd "C-c f n") 'flymake-goto-next-error)
                     (global-set-key (kbd "C-c f p") 'flymake-goto-prev-error)
                     (global-set-key (kbd "C-c f d") 'flymake-display-err-menu-for-current-line)))

                 (add-hook 'c++-mode-hook 'flymake-clang-c++-load)

                 (message "flymake... done"))
             (message "flymake seems not installed")))

     (when section-magit (message "magit...")
           ;; http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
           ;; This should disable the backend:
           ;; (remove-hook 'find-file-hooks 'vc-find-file-hook)
           ;; you might need a (require 'vc) before the above line to get the timing right. Or perhaps wrap it like so:
           ;; (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
           ;; to get the timing right.
           (require 'vc)
           (remove-hook 'find-file-hooks 'vc-find-file-hook)

           ;; (add-to-list 'load-path "~/.emacs.d/magit/")
           (if (require 'magit nil 'noerror)
               ;; (eval-after-load 'magit
               (progn
                 (require 'magit)
                 (global-set-key (kbd "C-c s") 'magit-status)))

           ;; http://readystate4.com/2011/02/22/emacs-changing-magits-default-diff-colors/
           (eval-after-load 'magit
             '(progn
                (set-face-foreground 'magit-diff-add "green3")
                (set-face-foreground 'magit-diff-del "red3")
                (when (not window-system)
                  (set-face-background 'magit-item-highlight "black"))))

           (message "magit... done"))

     (when section-gtags (message "gtags...")
           (if (not (file-exists-p "~/.emacs.d/el-get/gtags.el"))
               (url-copy-file "https://github.com/inlinechan/EmacsDot/raw/master/.emacs.d/gtags.el" "~/.emacs.d/el-get/gtags.el"))

           (if (require 'gtags nil 'noerror)
               (progn
                 (autoload 'gtags-mode "gtags" "" t)
                 (global-set-key (kbd "C-c C-f") 'gtags-find-file)
                 (global-set-key (kbd "C-c g f") 'gtags-find-file)
                 (global-set-key (kbd "C-c g t") 'gtags-find-tag-from-here)
                 (global-set-key (kbd "C-c g p") 'gtags-find-pattern)
                 (global-set-key (kbd "C-c g r") 'gtags-find-rtag)
                 (global-set-key (kbd "C-c g l") 'gtags-find-symbol)
                 (message "gtags... done"))))

     (when section-jedi (message "jedi...")
           ;; (if (require 'jedi nil 'noerror)
           (eval-after-load 'jedi
               (progn
                 (setq jedi:setup-keys t)
                 (setq jedi:key-goto-definition (kbd "C-c ."))
                 (require 'jedi)
                 (add-hook 'python-mode-hook 'jedi:setup)
                 (add-hook 'python-mode-hook 'jedi:ac-setup)
                 (message "jedi... done"))))

     (when section-qml-mode (message "qml-mode...")
           (eval-after-load 'qml-mode
               (progn
                 (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
                 (message "qml-mode... done"))))

     (when section-gdb-ui (message "gdb-ui...")
           (if (not (file-exists-p "~/.emacs.d/el-get/gdb-ui.el"))
               (url-copy-file "https://raw.github.com/inlinechan/gdb-ui/master/gdb-ui.el" "~/.emacs.d/el-get/gdb-ui.el"))

           (require 'gdb-ui nil 'noerror)
           (message "gtags... done"))

     (when section-gnuplot-mode (message "gnuplot-mode...")
           (if (not (file-exists-p "~/.emacs.d/el-get/gnuplot-mode.el"))
               (url-copy-file "https://raw.github.com/mkmcc/gnuplot-mode/master/gnuplot-mode.el" "~/.emacs.d/el-get/gnuplot-mode.el"))
           (require 'gnuplot-mode)
           (setq auto-mode-alist
                 (append '(("\\.gp$" . gnuplot-mode)
                           ("\\.gnu$" . gnuplot-mode))
                         auto-mode-alist))
           (message "gnuplot-mode... done"))
     )) ;; end of eval-after-load 'el-get
