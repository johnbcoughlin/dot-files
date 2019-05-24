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
        ;(add-hook 'pre-command-hook 'hypertex-clear-overlay-at-point)
        ;(add-hook 'post-command-hook 'hypertex-rerender-at-point)
        ;(add-hook 'post-self-insert-hook 'hypertex-rerender-at-point)
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

(defun hypertex-motion-wrapper (body)
  (progn
    (funcall body)
    (let ((overlays (org--list-latex-overlays (point) (or (mark) (point)))))
      (if overlays
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
               (atom (hypertex-select-atoms pt count tex))
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
