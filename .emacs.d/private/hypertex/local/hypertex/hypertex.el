(require 'evil)
(require 'evil-common)
(require 'latex)
(require 'tex)
(require 'libhypertex)

(defvar hypertex-latex-preamble
  "
\\usepackage{rcurs}
\\definecolor{fg}{rgb}{ fg }
\\definecolor{bg}{rgb}{ bg }
\\pagecolor{bg}{\\color{fg}}
")

(defvar hypertex-latex-command-line
  "xelatex -no-pdf")

;; Set up the renderer
(setq libhypertex-renderer
      (libhypertex-start-renderer hypertex-latex-preamble 1))

(define-minor-mode hypertex-mode
  "Toggle HyperLaTeX mode."
  nil
  " HyperTeX"
  :keymap (make-sparse-keymap)
  (if hypertex-mode
      (progn
        ;(add-hook 'pre-command-hook 'hypertex-clear-overlay-at-point)
        ;(add-hook 'post-command-hook 'hypertex-rerender-at-point)
        ;(add-hook 'post-self-insert-hook 'hypertex-rerender-at-point)
        )
    ;(remove-hook 'post-command-hook 'hypertex-rerender-at-point)
    ;(remove-hook 'pre-command-hook 'hypertex-clear-overlay-at-point)
    ;(remove-hook 'post-self-insert-hook 'hypertex-rerender-at-point)
    ))

(defun hypertex-clear-overlay-at-point ()
  (let* ((ovs (overlays-at (point)))
         (ov (if ovs (car ovs) nil)))
    (if ov (delete-overlay ov) ())))

(defun hypertex--testtest ()
  (let* ((frag (hypertex-latex-fragment-at-point))
        (ov (hypertex--get-or-create-overlay frag)))
    (hypertex--render-overlay frag ov)))

(defun hypertex--get-or-create-overlay (frag)
  (let* ((beg (org-element-property :begin frag))
         (end (org-element-property :end frag))
         (overlays (cl-remove-if-not
                    (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-hypertex-overlay))
                    (overlays-in beg end)))
         (overlay (if overlays (car overlays) nil)))
    (if overlay overlay
      (let ((ov (make-overlay beg end)))
        (progn (overlay-put ov 'org-overlay-type 'org-hypertex-overlay)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'hypertex-overlay-id (sha1 (org-element-property :value frag)))
               ov)))))

(defun hypertex--render-overlay (frag ov)
  (let* ((img-file (hypertex--overlay-image-file ov))
         (tex (org-element-property :value frag))
         (tex (hypertex--set-colors tex)))
    (progn
      (libhypertex-render-tex libhypertex-renderer (point) (mark) tex img-file)
      (overlay-put ov
                   'display
                   (list 'image :type 'svg :file img-file :ascent 'center)))))

(defun hypertex--set-colors (tex)
  (let* ((fg (face-attribute (face-at-point) :foreground nil 'default))
         (bg (face-attribute (face-at-point) :background nil 'default)))
    (progn
      (message "%s" fg)
      (format "\n
\\definecolor{fg}{rgb}{%s}\n
\\definecolor{bg}{rgb}{%s}\n
\\pagecolor{bg}\n
{\\color{fg}\n
%s
\n}\n"
              (org-latex-color-format fg)
              (org-latex-color :background)
              tex))))

(defun hypertex--overlay-image-file (ov)
  (let* ((id (overlay-get ov 'hypertex-overlay-id))
         (file (buffer-file-name (buffer-base-buffer)))
         (parent-dir (if (or (not file) (file-remote-p file))
                  temporary-file-directory
                  default-directory))
         (dir (concat parent-dir org-preview-latex-image-directory))
         (img_file (concat dir (format "org-ltximg_%s.svg" id))))
    img_file))

(defun hypertex-latex-fragment-at-point ()
  "Returns the LaTeX fragment at point, or nil if none"
  (if (org-inside-LaTeX-fragment-p)
      (let ((ctx (org-element-context)))
        (if (eq 'latex-fragment (org-element-type ctx))
            ctx
          nil))
    nil))

(defun hypertex--overlay-at-point ()
  (car (cl-remove-if-not
        (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-hypertex-overlay))
        (overlays-at (point)))))

(evil-define-text-object hypertex-atom-text-object (count)
  ""
  (libhypertex-select-atoms (point) count (org-element-property :value (org-element-context))))

(defun hypertex-motion-wrapper (body)
  (progn
    (funcall body)
    (let ((overlay (hypertex--overlay-at-point)))
      (if overlay
          (progn
            (setq disable-point-adjustment t)
            (setq cursor-type nil)
            )
        (setq cursor-type t)))))

(defun hypertex--combined-motion-loop (count hypertex-motion evil-motion)
  (evil-motion-loop (unit count)
    (unless
        (progn
          (message "calling hypertex %d" unit)
          (funcall hypertex-motion unit))
      (progn
        (message "calling evil %d" unit)
        (funcall evil-motion unit)))))

(defun hypertex--move-atoms-begin (count)
  (let ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (let* ((tex (org-element-property :value frag))
               (begin (org-element-property :begin frag))
               (pt (- (point) begin))
               (atom (libhypertex-select-atoms pt count tex))
               )
          (if atom (let ((beg (+ (car atom) begin)))
                     (goto-char beg))
            nil))
      nil)))

(evil-define-motion evil-hypertex-atom-forward (count &optional crosslines noerror)
  (hypertex-motion-wrapper
   (lambda ()
     (hypertex--combined-motion-loop
      (or count 1)
      (lambda (ct) (hypertex--move-atoms-begin ct))
      (lambda (ct) (evil-forward-char ct))))))

(evil-define-motion evil-hypertex-atom-backward (count &optional crosslines noerror)
  "Move forward by COUNT symbols, as they appear in the rendered LaTeX equation"
  (hypertex-motion-wrapper
   (lambda ()
     (hypertex--combined-motion-loop
      (or count 1)
      (lambda (ct) (hypertex--move-atoms-begin (- ct)))
      (lambda (ct) (evil-backward-char ct))))))

(defvar evil-hypertex-normal-map (make-sparse-keymap))
(defvar evil-hypertex-motion-map (make-sparse-keymap))

(evil-define-minor-mode-key 'motion 'hypertex-mode "l" 'evil-hypertex-atom-forward)
(evil-define-minor-mode-key 'motion 'hypertex-mode "h" 'evil-hypertex-atom-backward)

(defvar evil-hypertex-around-map (make-sparse-keymap))
(defvar evil-hypertex-inside-map (make-sparse-keymap))

(define-key evil-hypertex-around-map "s" 'hypertex-subscript-around)
(define-key evil-hypertex-inside-map "s" 'hypertex-subsuperscript-inside)

(evil-define-key 'visual hypertex-mode-map
  "a" evil-hypertex-around-map
  "i" evil-hypertex-inside-map)

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
