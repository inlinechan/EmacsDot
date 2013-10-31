(setq auto-mode-alist
      (append '(("\\.gp$"  . gnuplot-mode)
                ("\\.gnu$" . gnuplot-mode))
              auto-mode-alist))
