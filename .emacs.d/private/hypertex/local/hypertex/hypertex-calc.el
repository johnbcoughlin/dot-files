(require 'evil)
(require 'evil-common)
(require 'latex)
(require 'tex)
(require 'libhypertex)

;; Create or update an overlay on every calc stack entry
(defun hypertex--create-line-overlays ()
  (goto-char (point-min))
  ; Skip the header line --- Emacs Calculator Mode ---
  (forward-line 1)
  (while (not (eobp))
    (hypertex--overlay-line)
    (forward-line 1)))

;; Create or update an overlay on the line at point
(defun hypertex--overlay-line ()
  (let ((line-start (if calc-line-numbering (+ (line-beginning-position) 4)
                      (line-beginning-position)))
        (line-end (line-end-position)))
    (put-text-property line-start line-end
                       'font-lock-face '(:foreground "red"))))
