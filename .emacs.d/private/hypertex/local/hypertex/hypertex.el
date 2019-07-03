(require 'evil)
(require 'latex)
(require 'tex)
(require 'libhypertex)

(defvar hypertex-latex-preamble
  "
")

(defvar hypertex--last-overlay nil)
(defvar hypertex--last-frag nil)
;; Sidechannel used to store the value of calc-line-numbering, so that it is usable
;; in our hook without depending on hook execution ordering.
(defvar hypertex--calc-line-numbering nil)

;; Set up the renderer
(defun hypertex-renderer-start ()
  (setq libhypertex-renderer
        (libhypertex-start-renderer
         hypertex-latex-preamble
         10)))

(defun hypertex-renderer-stop ()
  (progn
    (libhypertex-stop-renderer libhypertex-renderer)
    (setq libhypertex-renderer nil)))

(define-minor-mode hypertex-mode
  "Toggle HyperLaTeX mode."
  nil
  " HyperTeX"
  :keymap (make-sparse-keymap)
  (if hypertex-mode
      (progn
        (add-hook 'pre-command-hook 'hypertex--precommand)
        (add-hook 'post-command-hook 'hypertex--postcommand)
        (add-hook 'post-self-insert-hook 'hypertex--postcommand)
        (hypertex-renderer-start)
        )
    (hypertex-renderer-stop)
    (remove-hook 'pre-command-hook 'hypertex--precommand)
    (remove-hook 'post-command-hook 'hypertex--postcommand)
    (remove-hook 'post-self-insert-hook 'hypertex--postcommand)
    ))

(defun hypertex--precommand ()
  (progn
    (setq hypertex--calc-line-numbering calc-line-numbering)
    (if (and (string= "*Calculator*" (buffer-name))
             (string-prefix-p "calc" (symbol-name this-command)))
        (let ((stack-size (calc-stack-size))
              (selections-enabled (if calc-use-selections 1 0))
              (inhibit-message t)
              )
          (progn
            (dotimes (idx (calc-stack-size))
              (progn
                (calc-enable-selections 0)
                (calc-roll-down (calc-stack-size))
                (calc-enable-selections 1)
                (if (calc-top-selected)
                    (calc-rewrite-selection "hideselected" 1)
                  ())
                ))
            (calc-enable-selections selections-enabled)
            ))
      )
    ))

(defun hypertex--postcommand ()
  (progn
    (hypertex--render-just-exited-overlay)
    ;; This function will override the variables used by the previous one
    (if (and (string= "*Calculator*" (buffer-name))
             (string-prefix-p "calc" (symbol-name this-command)))
        (let ((stack-size (calc-stack-size))
              (selections-enabled (if calc-use-selections 1 0))
              (inhibit-message t)
              )
          (progn
            (dotimes (idx (calc-stack-size))
              (progn
                (calc-enable-selections 0)
                (calc-roll-down (calc-stack-size))
                (calc-enable-selections 1)
                (if (calc-top-selected)
                    (calc-rewrite-selection "liftselected" 1)
                  (calc-rewrite-selection "hideselected" 1))
                  ))
            (calc-enable-selections selections-enabled)
            (hypertex--create-line-overlays)
            ))
      ())))

(defun hypertex--render-overlay-at-point ()
  (let ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (let ((ov (hypertex--get-or-create-overlay-at-frag frag)))
          (if ov
              (progn
                (hypertex--render-overlay-at-frag frag ov)
                (setq hypertex--last-overlay ov
                      hypertex--last-frag frag)
                )
            ()))
      ())))

(defun hypertex--render-just-exited-overlay ()
  (if (and (not (hypertex-latex-fragment-at-point))
           hypertex--last-overlay
           hypertex--last-frag)
      (progn
        (hypertex--render-overlay-at-frag hypertex--last-frag hypertex--last-overlay)
        (setq hypertex--last-overlay nil
              hypertex--last-frag nil))))

(defun hypertex--modification-hook (ov after beg end &optional len)
  (hypertex--render-overlay-at-point))

(defun hypertex--get-or-create-overlay-at-frag (frag)
  (let* ((beg (org-element-property :begin frag))
         (end (save-excursion
                (goto-char (org-element-property :end frag))
                (skip-chars-backward " \r\t\n")
                (point))))
    (hypertex--get-or-create-overlay beg end)))

(defun hypertex--get-or-create-overlay (beg end)
  (let* ((overlays (cl-remove-if-not
                    (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-hypertex-overlay))
                    (overlays-in beg end)))
         (overlay (if overlays (car overlays) nil)))
    (if overlay
        overlay
      (let ((ov (make-overlay beg end)))
        (progn (overlay-put ov 'org-overlay-type 'org-hypertex-overlay)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'modification-hooks (list 'hypertex--modification-hook))
               (overlay-put ov 'hypertex-overlay-id (sha1 (buffer-substring beg end)))
               ov)))))

(defun hypertex--render-overlay-at-frag (frag ov)
  (let* ((tex (org-element-property :value frag))
         (fg (hypertex-latex-color :foreground))
         (cursor-color (hypertex-latex-color-format (face-background 'cursor)))
         )
    (let ((img-file
               (libhypertex-render-tex
                libhypertex-renderer
                fg
                cursor-color
                tex
                "/Users/jack/org/ltximg")))
          (progn
            (if img-file
                (overlay-put ov
                             'display
                             (list 'image
                                   :type 'svg
                                   :file img-file
                                   :ascent 'center
                                   :scale 0.34
                                   ))
              ())
            (setq disable-point-adjustment t)))))

(defun hypertex--render-overlay-at (tex ov)
  (let* ((fg (hypertex-latex-color :foreground))
         (cursor-color (hypertex-latex-color-format (face-background 'cursor)))
         (img-file
          (libhypertex-render-tex
           libhypertex-renderer
           fg
           cursor-color
           tex
           "/Users/jack/org/ltximg")))
    (progn
      (if img-file
          (overlay-put ov
                       'display
                       (list 'image
                             :type 'svg
                             :file img-file
                             :ascent 'center
                             :scale 0.34
                             :margin 4
                             )))
      (setq disable-point-adjustment t))))

(defun hypertex-latex-color (attr)
  "Return a RGB color for the LaTeX color package."
  (hypertex-latex-color-format (face-attribute 'default attr nil)))

(defun hypertex-latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s %s %s"
         (mapcar 'org-normalize-color
                 (color-values color-name))))

(defun hypertex--overlay-image-file (ov)
  (let* ((id (overlay-get ov 'hypertex-overlay-id))
         (file (buffer-file-name (buffer-base-buffer)))
         (parent-dir (if (or (not file) (file-remote-p file))
                  temporary-file-directory
                  default-directory))
         (dir (concat parent-dir org-preview-latex-image-directory))
         (img_file (concat dir (format "org-ltximg_%s.png" id))))
    img_file))

(defun hypertex-latex-fragment-at-point ()
  "Returns the LaTeX fragment at point, or nil if none"
  (let ((ctx (org-element-context)))
    (if (or (eq 'latex-fragment (org-element-type ctx))
            (eq 'latex-environment (org-element-type ctx)))
        ctx
      nil)))

(defun hypertex--overlay-at-point ()
  (car (cl-remove-if-not
        (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-hypertex-overlay))
        (overlays-at (point)))))

(defun hypertex--remove-overlay-at-point ()
  (let ((ov (hypertex--overlay-at-point)))
    (if ov
        (delete-overlay ov)
      ())))

(evil-define-text-object hypertex-atom-text-object (count)
  ""
  (libhypertex-select-atoms (point) count (org-element-property :value (org-element-context))))

(defun hypertex-overlay-wrapper (body)
  (progn
    ;; Attempt the operation
    (condition-case nil
        (funcall body)
      ;; If it fails, remove the overlay at point if any
      (error (progn
                       (message "error happened!")
                       (hypertex--remove-overlay-at-point))))
    (let ((overlay (hypertex--overlay-at-point)))
      (if overlay
          (progn
            (setq disable-point-adjustment t)
            (setq cursor-type nil))
        (setq cursor-type t)))))

;; Create or update an overlay on every calc stack entry
(defun hypertex--create-line-overlays ()
  (if (string= calc-language "latex")
      (progn
        (goto-char (point-min))
        ; Skip the header line --- Emacs Calculator Mode ---
        (forward-line 1)
        (while (not (eobp))
          (hypertex--overlay-line)
          (forward-line 1)))
    ()))

;; Create or update an overlay on the line at point
(defun hypertex--overlay-line ()
  (if (string=
       "."
       (string-trim (buffer-substring
                     (line-beginning-position)
                     (line-end-position))))
      ()
    (let* ((line-start (if hypertex--calc-line-numbering
                           (+ (line-beginning-position) 4)
                         (line-beginning-position)))
           (line-end (line-end-position))
           (line-contents (buffer-substring line-start line-end))
           (ov (hypertex--get-or-create-overlay line-start line-end))
           (tex (format "\\[ %s \\]" line-contents)))
      (progn
        (move-overlay ov line-start line-end)
        (hypertex--render-overlay-at tex ov)
        )
          ;(add-face-text-property line-start line-end '(:foreground "green"))
          )))

(defun hypertex--marker-within-frag (marker frag)
  (if marker
      (let* ((begin (org-element-property :begin frag))
             (pt (- marker begin)))
        pt)
    nil))

;; Activate all formulas for embedded mode
(defun hypertex--activate-all ()
  (interactive)
  ;; Straight out of org.el
  (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
         (cnt 0))
    (goto-char (point-min))
    (while (re-search-forward math-regexp (point-max) t)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        (when (memq type '(latex-environment latex-fragment))
          (calc-embedded nil))))))

;; Activate the formula at point with calc Embedded mode.
(defun hypertex-activate-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :begin frag))
          ;; Set a bookmark to jump back to
          (bookmark-set "hypertex-formula")
          (calc-embedded nil)
          (goto-char (org-element-property :begin frag))
          (calc)))))

(defun hypertex-accept-formula ()
  (interactive)
  (spacemacs/alternate-window)
  (bookmark-jump "hypertex-formula")
  (calc-embedded t)
  )

;;;###autoload
(defun turn-on-hypertex-mode ()
  "Enable hypertex-mode in current buffer."
  (interactive "")
  (hypertex-mode 1))

;;;###autoload
(defun turn-off-hypertex-mode ()
  "Disable hypertex-mode in current buffer."
  (interactive "")
  (hypertex-mode -1))

(provide 'hypertex)
