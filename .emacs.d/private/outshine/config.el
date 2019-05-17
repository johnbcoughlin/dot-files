;; Use outshine instead of outline-minor-mode
(add-hook 'outline-minor-mode-hook 'outshine-mode)

;; Enable outlines in all source code
(add-hook 'prog-mode-hook 'outline-minor-mode)
