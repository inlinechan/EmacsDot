(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
       )
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(eval-after-load 'el-get
  '(progn

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; el-get :: now set our own packages
     (setq
      my:el-get-packages
      '(auto-complete
        auto-complete-clang
        auto-complete-css
        auto-complete-emacs-lisp
        auto-complete-etags
        bookmark+
        cmake-mode
        color-theme-solarized
        css-mode-simple
        ;; flymake-cursor
        flycheck
        highlight-chars
        gdict
        ;; ggtags
        google-c-style
        jquery-doc
        js2-mode
        magit
        org-mode
        popup
        popup-pos-tip
        pos-tip
        psvn
        qmake-mode
        org-octopress
        orglue
        qml-mode
        rtags
        slime
        yasnippet
        ;; tern - requirement
        ;; $ apt-get install nodejs, npm
        ;; $ npm install acorn glob minimatch
        tern
        web-mode
        org-reveal
        ))

     ;; Optional packages
     ;; For example, emacs-w3m is available if w3m exist
     (setq mode-alist
           '(("cvs"          . emacs-w3m)                 ; w3m
             ("markdown"     . markdown-mode)             ; markdown
             ("gnuplot"      . gnuplot-mode)              ; gnuplot
             ("virtualenv"   . jedi)                      ; virtualenv
             ("dot"          . graphviz-dot-mode)         ; graphviz
             ;; https://bitbucket.org/jonwaltman/pydoc-info/
             ("/usr/share/info/python.info" . pydoc-info) ; pydoc-info(manual install)
             ))

     (defun packages-exist-p (package-list)
       (if (not package-list)
           t                            ; ignore nil elmt to do and operation
         (and (executable-find (car package-list))
              (packages-exist-p (cdr package-list)))))

     (dolist (mode-item mode-alist)
       (when
           (or (and (car mode-item) (packages-exist-p (split-string (car mode-item))))
               (file-exists-p (car mode-item)))
         (add-to-list 'my:el-get-packages (cdr mode-item))))

     (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

     (setq my:el-get-packages
           (append
            my:el-get-packages
            (loop for src in el-get-sources collect (el-get-source-name src))))

     ;; install new packages and init already installed packages
     (el-get 'sync my:el-get-packages)))

(provide 'hc-el-get)
