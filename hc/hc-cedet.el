;; TODO: not used for a long time
(require 'cedet)
;; cedet

;; http://www.emacswiki.org/emacs/collectionofemacsdevelopmentenvironmenttools
(setq byte-compile-warnings nil)

;; load cedet.
;; see cedet/common/cedet.info for configuration details.
;; (load-file "~/.emacs.d/cedet-1.0.1/common/cedet.el")

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

