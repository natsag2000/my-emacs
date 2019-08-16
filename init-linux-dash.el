(use-package helm-dash
  :ensure t
  :init
  (progn
    (setq helm-dash-common-docsets
          '("Elixir"
            "Erlang"
            "Bash"
            "Emmet"
            "JavaScript"))))

(provide 'init-linux-dash)
