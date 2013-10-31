(setq auto-mode-alist
      (append
       '(("\\.pro$" . qmake-mode)
         ("\\.pri$" . qmake-mode)
         ("\\.prf$" . qmake-mode))
       auto-mode-alist))
