;;; Package --- Summary

;;; Commentary:

;;; Code:

(require 'find-dired)

(defvar webos-find-recipes-args nil
  "Last arguments given to `find' by \\[webos-find-recipes].")

;; History of webos-find-args values entered in the minibuffer.
(defvar webos-find-recipes-args-history nil)

(defun webos-find-recipe-candidates ()
  "Find bitbake recipe candidates in the subdirectories recursively."
  (let ((wtop (string-trim-right (shell-command-to-string "wtop do_not_cd")))
        (find-command (concat "find meta\* -type f -regex "
                              "\".*\\(bb\\|bbclass\\|bbappend\\|inc\\)$\" "
                              "| awk -F'/' '{print \$NF}'")))
    (cd wtop)
    (if webos-find-recipes-args-history
        webos-find-recipes-args-history
      (setq webos-find-recipes-args-history
            (split-string (shell-command-to-string find-command))))))

(defun webos-find-recipes (pattern)
  "Find bitbake recipes with matching PATTERN."
  (interactive
   (list (completing-read "recipe: " (webos-find-recipe-candidates))))

  (let* ((buffer-name "*Find*")
         (wtop (string-trim-right (shell-command-to-string "wtop do_not_cd")))
         (recipe-buffer (get-buffer-create buffer-name))
         (find-command nil)
         (dir (expand-file-name wtop)))
    (if (and (not (string-empty-p wtop)) (string-match wtop (expand-file-name default-directory)))
        (progn
          (switch-to-buffer (get-buffer-create buffer-name))

          (let ((find (get-buffer-process (current-buffer))))
            (when find
              (if (or (not (eq (process-status find) 'run))
                      (yes-or-no-p "A `find' process is running; kill it? "))
                  (condition-case nil
                      (progn
                        (interrupt-process find)
                        (sit-for 1)
                        (delete-process find))
                    (error nil))
                (error "Cannot have two processes in `%s' at once" (buffer-name)))))

          (widen)
          (kill-all-local-variables)
          (setq buffer-read-only nil)
          (erase-buffer)

          (cd wtop)
          (setq find-command (concat "find meta\* -type f -ls | egrep \"(.*/)"
                                     pattern
                                     "$\""))
          (shell-command find-command (current-buffer) "*Message*")

          (dired-mode dir (cdr find-ls-option))
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (current-local-map))
            (define-key map "\C-c\C-k" 'kill-find)
            (define-key map "g" nil)
            (use-local-map map))
          (make-local-variable 'dired-sort-inhibit)

          (set (make-local-variable 'revert-buffer-function)
               `(lambda (ignore-auto noconfirm)
                  (find-dired ,dir ,find-args)))

          (if (fboundp 'dired-simple-subdir-alist)
              ;; will work even with nested dired format (dired-nstd.el,v 1.15
              ;; and later)
              (dired-simple-subdir-alist)
            ;; else we have an ancient tree dired (or classic dired, where
            ;; this does no harm)
            (set (make-local-variable 'dired-subdir-alist)
                 (list (cons default-directory (point-min-marker)))))
          (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
          (setq buffer-read-only nil)
          ;; Subdir headlerline must come first because the first marker in
          ;; subdir-alist points there.
          (insert "  " dir ":\n")
          ;; Make second line a ``find'' line in analogy to the ``total'' or
          ;; ``wildcard'' line.
          (insert "  " find-command "\n")
          (setq buffer-read-only t)
          ;; (let ((proc (get-buffer-process (current-buffer))))
          ;;   (set-process-filter proc (function find-dired-filter))
          ;;   (set-process-sentinel proc (function find-dired-sentinel))
          ;;   ;; Initialize the process marker; it is used by the filter.
          ;;   (move-marker (process-mark proc) 1 (current-buffer)))
          (setq mode-line-process '(":%s"))
          )
      (message "Not in webos directory"))))

(setq auto-mode-alist
      (append
       '(
         ;; bitbake
         ("\\.bb$"       . python-mode)
         (".*meta.*\\.inc" . python-mode)
         ("\\.bbappend$" . python-mode)
         )
       auto-mode-alist))

(provide 'hc-webos)

;;; hc-webos ends here
