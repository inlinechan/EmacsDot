(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)
