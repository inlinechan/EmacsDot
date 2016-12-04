(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:key-goto-definition (kbd "C-c ."))
(require 'jedi)

(setq jedi:environment-root "jedi")  ; or any other name you like
(setq jedi:environment-virtualenv
      (append python-environment-virtualenv
              '("--python" "/usr/bin/python3")))

;; python-mode-hook
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:start-server)
