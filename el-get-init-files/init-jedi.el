(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:key-goto-definition (kbd "C-c ."))

(add-hook 'python-mode-hook
          (lambda ()
            (require 'python-environment)
            (require 'jedi)                          ; to expose `jedi:start-server'
            (setq jedi:environment-root "python3.5")  ; or any other name you like
            (setq jedi:environment-virtualenv
                  (append python-environment-virtualenv
                          '("--python"
                            (concat (file-name-as-directory python-environment-directory) jedi:environment-root "/bin/python"))))
            (setq jedi:server-command
                  (list (concat (file-name-as-directory python-environment-directory) jedi:environment-root "/bin/jediepcserver")))
            (jedi:setup)
            (jedi:ac-setup)
            (jedi:start-server)
            ))
