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

; Make babel results blocks lowercase
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

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(setq org-agenda-files (quote "~/Documents/org/todo.org.gpg"))

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

;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "STARTED(s@/!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "DEFERRED(x@)")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/org/todo.org.gpg" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("p" "Personal" entry (file+headline "~/Documents/org/todo.org.gpg" "Personal")
         "* TODO %?\n  %i\n  %a")
        ("w" "WAMTEST" entry (file+headline "~/Documents/org/todo.org.gpg" "WAMTEST")
         "* TODO %?\n  %i\n  %a")))

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 80)))

(define-key global-map "\C-cc" 'org-capture)

(provide 'hc-org)
;;; hc-org ends here
