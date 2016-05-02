(dolist (mode (list
               'sgml-mode-hook
               'web-mode-hook))
  (add-hook mode 'emmet-mode))
