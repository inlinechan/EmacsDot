(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:key-goto-definition (kbd "C-c ."))
(require 'jedi)

;; python-mode-hook
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
