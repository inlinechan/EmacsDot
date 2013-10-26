(setq auto-mode-alist
      (append
       '(("\\.pro$" . qmake-mode)
	 ("\\.pri$" . qmake-mode))
       auto-mode-alist))
