(require 'evil)
(require 'evil-common)
(require 'latex)
(require 'tex)
(require 'hypertex)

(define-minor-mode hypertex-mode
  "Toggle HyperLaTeX mode."
  nil
  " HyperTeX"
  :keymap (make-sparse-keymap)
  (if hypertex-mode
      (progn
        (add-hook 'pre-command-hook 'hypertex-clear-overlay-at-point)
        (add-hook 'post-command-hook 'hypertex-rerender-at-point)
        (add-hook 'post-self-insert-hook 'hypertex-rerender-at-point)
        )))

(defun hypertex-clear-overlay-at-point ()
  (let* ((ovs (overlays-at (point)))
         (ov (if ovs (car ovs) nil)))
    (if ov (delete-overlay ov) ())))

(defun hypertex-rerender-at-point ()
  (let ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (progn (message "creating new overlay")
               (hypertex-create-overlay-at-point frag)
               (message "rerendered"))
      ())))

(defun hypertex-create-overlay-at-point (frag)
        (let* ((beg (org-element-property :begin frag))
               (end (org-element-property :end frag))
               (overlays (overlays-at beg end))
               (overlay (if overlays (car overlays) nil))
               (ov (if overlay overlay (make-overlay beg end))))
            (overlay-put ov 'org-overlay-type 'org-hypertex-overlay)
            (overlay-put ov 'evaporate t)
            (overlay-put ov
                         'modification-hooks
                         (list (lambda (o _flag _beg _end &optional _l)
                                 (delete-overlay o))))
            (overlay-put ov 'display
                         (list 'image :type 'svg :file "~/org/ltximg/org-ltximg_f3f0c7413d70aea7f7f91f475be82bc340c37e29.svg" :ascent 'center))
      ))

(defun hypertex-latex-fragment-at-point ()
  "Returns the LaTeX fragment at point, or nil if none"
  (if (org-inside-LaTeX-fragment-p)
      (let ((ctx (org-element-context)))
        (if (eq 'latex-fragment (org-element-type ctx))
            ctx
          nil))
    nil))

(evil-define-text-object hypertex-atom-text-object (count)
  ""
  (hypertex-select-atoms (point) count (org-element-property :value (org-element-context))))

(evil-define-motion evil-hypertex-atom-forward (count &optional crosslines noerror)
  "Move forward by COUNT symbols, as they appear in the rendered LaTeX equation"
  (let ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (let* ((count (or count 1))
               (tex (org-element-property :value frag))
               (begin (org-element-property :begin frag))
               (pt (- (point) begin))
               (atom (hypertex-select-atoms pt count tex))
               (beg (+ (car atom) begin)))
          (goto-char beg))
      (evil-forward-char count crosslines noerror))))

(evil-define-motion evil-hypertex-atom-backward (count &optional crosslines noerror)
  "Move forward by COUNT symbols, as they appear in the rendered LaTeX equation"
  (let ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (let* ((ct (if count (- 0 count) -1))
               (tex (org-element-property :value frag))
               (begin (org-element-property :begin frag))
               (pt (- (point) begin))
               (atom (hypertex-select-atoms pt ct tex))
               (beg (+ (car atom) begin)))
          (goto-char beg))
      (evil-backward-char count crosslines noerror))))

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
