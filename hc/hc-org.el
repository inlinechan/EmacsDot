;;; package --- Summary
;; http://doc.norang.ca/org-mode.html#OrgBabel
;;; Commentary:

;;; Code:

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; babel specific
(if (file-exists-p "/usr/share/ditaa/ditaa.jar")
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
  (setq org-ditaa-jar-path (expand-file-name "~/.emacs.d/babel/ditaa.jar")))

(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/babel/plantuml.jar"))

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

;; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t)
         (C . t))))

;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(setq org-src-fontify-natively t)

(setq doc-view-conversion-refresh-interval 1)

;; Do not fold org-mode in ediff
;; http://web.archiveorange.com/archive/v/Fv8aAM6yHysyeOVrnWBE#uTlUCjbhZTHNL53
(add-hook 'ediff-prepare-buffer-hook 'f-ediff-prepare-buffer-hook-setup)
(defun f-ediff-prepare-buffer-hook-setup ()
  ;; specific modes
  (cond ((eq major-mode 'org-mode)
         (f-org-vis-mod-maximum))
        ;; room for more modes
        )
  ;; all modes
  (setq truncate-lines nil))
(defun f-org-vis-mod-maximum ()
  "Visibility: Show the most possible."
  (cond
   ((eq major-mode 'org-mode)
    (visible-mode 1)  ; default 0
    (setq truncate-lines nil)  ; no `org-startup-truncated' in hook
    (setq org-hide-leading-stars t))  ; default nil
   (t
    (message "ERR: not in Org mode")
    (ding))))

(defun hc/org-concat (path)
  "concatenate path with org-directory"
  (concat (file-name-as-directory org-directory) path))

(setq org-directory "~/Documents/org"
      org-agenda-file-refile (hc/org-concat "refile.org")
      org-agenda-file-personal (hc/org-concat "personal/todo.org")
      org-agenda-file-journal (hc/org-concat "personal/journal.org")
      org-agenda-file-emacs (hc/org-concat "personal/emacs.org")
      org-agenda-files (list
                        (hc/org-concat "personal")
                        (hc/org-concat "work")
                        org-directory))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "|"
                  "DONE(d!)" "CANCELED(c@)" "DEFERRED(x@)"))

      org-capture-templates
      '(
        ("t" "Todo" entry (file+headline org-agenda-file-refile "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("p" "Personal" entry (file+headline org-agenda-file-personal "Personal Tasks")
         "* TODO %? :PERSONAL:\n  %i\n  %a")
        ("c" "C++" entry (file+headline org-agenda-file-personal "C++")
         "* TODO %? :CPP:\n  %i\n  %a")
        ("e" "Emacs" entry (file+headline org-agenda-file-emacs "Emacs")
         "* TODO %? :EMACS:\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree org-agenda-file-journal)
         "* %?" :empty-lines 1)
        ("w" "Work" entry (file+headline org-agenda-file-emacs "Work")
         "* TODO %? :EMACS:\n  %i\n  %a")
        )

      org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3))

      org-completion-use-ido nil
      org-refile-use-outline-path nil
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t)

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
    (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 80)))

(define-key global-map "\C-cc" 'org-capture)

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let ((my-pre-bg (face-background 'default)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head
            (format "<style type=\"text/css\">\n pre.src { background-color: %s;}</style>\n" my-pre-bg)))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

(setq org-clock-in-switch-to-state
      #'(lambda (state)
          (when (not (string= state "STARTED"))
            "STARTED"))

      org-clock-out-switch-to-state
      #'(lambda (state)
          (when (not (string= state "WAITING"))
            "WAITING"))

      org-clock-idle-time 25
      org-clock-clocked-in-display 'both)

(provide 'hc-org)
;;; hc-org ends here
