(require 'flymake)
(require 'flymake-cursor)

;; (defun flymake-clang-c++-init ()
;;   ;; ediff control(at the bottom while emacs running) buffer-file-name is nil
;;   (if (not buffer-file-name)
;;       (flymake-mode-off)
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		       'flymake-create-temp-inplace))
;; 	   (local-file (file-relative-name
;; 			temp-file
;; 			(file-name-directory buffer-file-name)))
;; 	   (git-root-dir (my-git-root)))

;;       (if (or (string-match "\.index$" local-file)
;; 	      ;; (not (string-match ".cpp$" local-file))
;; 	      (not git-root-dir))
;; 	  (flymake-mode-off)
;; 	(if (file-exists-p (concat
;; 			    git-root-dir "Tools/Scripts/check-webkit-style"))
;; 	    (list "bash" (list "flymake-check-webkit-style.sh" git-root-dir local-file))
;; 	  (list "clang++" (list "-fsyntax-only" "-fno-color-diagnostics" temp-file)))))))

;; (defun flymake-clang-c++-load ()
;;   (interactive)
;;   ;; (message "#### %s " buffer-file-name)
;;   (unless (eq buffer-file-name nil)
;;     ;; (unless (or (eq buffer-file-name nil)
;;     ;;             (not (string-match "\.cpp$" buffer-file-name)))
;;     (add-to-list 'flymake-allowed-file-name-masks
;; 		 '("\\.cpp" flymake-clang-c++-init))
;;     (add-to-list 'flymake-allowed-file-name-masks
;; 		 '("\\.cc" flymake-clang-c++-init))
;;     (add-to-list 'flymake-allowed-file-name-masks
;; 		 '("\\.h" flymake-clang-c++-init))
;;     (flymake-mode t)
;;     (global-set-key (kbd "C-c f n") 'flymake-goto-next-error)
;;     (global-set-key (kbd "C-c f p") 'flymake-goto-prev-error)
;;     (global-set-key (kbd "C-c f d") 'flymake-display-err-menu-for-current-line)))

;; (add-hook 'c++-mode-hook 'flymake-clang-c++-load)
