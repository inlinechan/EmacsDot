(add-hook 'c++-mode-hook (lambda ()
               (add-to-list 'ac-sources 'ac-source-clang)))
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
(add-hook 'c-mode-hook 'my-ac-cc-mode-setup)
