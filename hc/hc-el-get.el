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
        color-theme
        ;; flymake-cursor
        flycheck
        highlight-chars
        hc-gdb-ui
        hc-gtags
        hc-gud
        gdict
        gnuplot-mode
        graphviz-dot-mode
        jedi
        jquery-doc
        js2-mode
        magit
        markdown-mode
        popup
        popup-pos-tip
        pos-tip
        psvn
        qmake-mode
        qml-mode
        yasnippet
        ))

     (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

     (setq my:el-get-packages
           (append
            my:el-get-packages
            (loop for src in el-get-sources collect (el-get-source-name src))))

     ;; install new packages and init already installed packages
     (el-get 'sync my:el-get-packages)))

(provide 'hc-el-get)
