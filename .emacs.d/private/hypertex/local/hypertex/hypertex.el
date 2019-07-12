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
    (hypertex--remove-overlays)
    ))

(defun hypertex--precommand ()
  (progn
    (setq hypertex--calc-line-numbering calc-line-numbering)
    ))

(defun hypertex--postcommand ()
  (progn
    (hypertex--render-just-exited-overlay)
    ;; This function will override the variables used by the previous one
    (hypertex--create-line-overlays)
))

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
  (condition-case nil
      (hypertex--render-overlay-at-point)
    (error (progn
             (message "error happened!")
             (hypertex--remove-overlay-at-point)))))

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
               (overlay-put ov 'priority -60)
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
                             :margin 2
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
  (if (and (string= calc-language "latex")
           (string= "*Calculator*" (buffer-name)))
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
           (selected-line-contents (hypertex--lift-selection line-contents line-start line-end))
           (ov (hypertex--get-or-create-overlay line-start line-end))
           (tex (format "\\[ %s \\]" selected-line-contents)))
      (progn
        (message selected-line-contents)
        (move-overlay ov line-start line-end)
        (hypertex--render-overlay-at tex ov))
          )))

(defun hypertex--lift-selection (text line-start line-end)
  (save-excursion
    (progn
      (goto-char line-start)
      (let ((min)
            (max))
        (progn
          (while (and (< (point) line-end)
                      (not max))
            (let* ((pt (point))
                   (face (get-text-property pt 'face))
                   (selected (equal 'calc-selected-face face)))
              (progn
                (if min
                    (if (not selected)
                        (setq max pt)
                      ())
                  (if selected
                      (setq min pt)
                    ()))
                (forward-char))))
          (if min
              (progn
                (if (not max)
                    (setq max (point))
                  ())
                (setq min (- min line-start))
                (setq max (- max line-start))
                (concat
                 (substring text 0 min)
                 "\\colornucleus{red}{"
                 (substring text min max)
                 "}"
                 (substring text max))
                )
            text)
        )
      ))))


(defun hypertex-hide-overlay-at-point ()
  (interactive)
  (let* ((overlays (overlays-at (point)))
         (overlays (seq-filter (lambda (ov)
                                 (eq (overlay-get ov 'org-overlay-type) 'org-hypertex-overlay))
                               overlays)))
    (if (eq 0 (length overlays))
        (hypertex--render-overlay-at-point)
      (delete-overlay (car overlays)))))

(defun hypertex--remove-overlays ()
  (with-current-buffer "*Calculator*"
    (dolist (ov (overlays-in (point-min) (point-max)))
      (delete-overlay ov))))

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

(defun hypertex-next-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point))
         (math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
         )
    (progn
      (if frag
          (progn
            (goto-char (org-element-property :end frag))
            (forward-char))
        ())
      (re-search-forward math-regexp (point-max) t)
      (let* ((frag (hypertex-latex-fragment-at-point))
             (begin (org-element-property :begin frag)))
        (goto-char begin))
      (setq disable-point-adjustment t))))

(defun hypertex-prev-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point))
         (math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"))
    (progn
      (if frag
          (progn
            (goto-char (org-element-property :begin frag))
            (backward-char))
        ())
      (re-search-backward math-regexp (point-min) t)
      (if (eq ?$ (char-after (point)))
          ;; Go backwards a character because frag-at-point doesn't work on the closing $
          (backward-char))
      (let* ((frag (hypertex-latex-fragment-at-point))
             (begin (org-element-property :begin frag)))
        (goto-char begin))
      (setq disable-point-adjustment t))))

;; Activate the formula at point with calc Embedded mode.
(defun hypertex-activate-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :begin frag))
          ;; Set a bookmark to jump back to
          (forward-char)
          (bookmark-set "hypertex-formula")
          (calc-embedded nil)
          (goto-char (org-element-property :begin frag))
          (hypertex--render-overlay-at-point)
          (calc)))))

(defun hypertex-accept-formula ()
  (interactive)
  (spacemacs/alternate-window)
  (bookmark-jump "hypertex-formula")
  (calc-embedded t)
  (let ((frag (hypertex-latex-fragment-at-point)))
    (goto-char (org-element-property :end frag))))

(defun hypertex-append-inline-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :end frag))
          (save-excursion (insert " "))
          )))
  (let ((calc-embedded-open-new-formula "$")
        (calc-embedded-close-new-formula "$"))
    (progn
      (calc-embedded-new-formula)
      (bookmark-set "hypertex-formula")
      (hypertex--render-overlay-at-point)
      (calc))))

(defun hypertex-insert-display-formula ()
  (interactive)
  (evil-insert-newline-below)
  (let ((calc-embedded-open-new-formula "\\[ ")
        (calc-embedded-close-new-formula " \\]"))
    (progn
      (calc-embedded-new-formula)
      (bookmark-set "hypertex-formula")
      (hypertex--render-overlay-at-point)
      (calc))))

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

(defhydra hypertex-hydra (:color red)
  "foo"
  ("h" hypertex-hide-overlay-at-point "show/hide overlays")
  ("n" hypertex-next-formula "next")
  ("p" hypertex-prev-formula "prev")
  ("r" hypertex-activate-formula "replace" :color blue)
  ("a" hypertex-append-inline-formula "Append $formula$" :color blue)
  ("o" hypertex-insert-display-formula "Insert \\[ display formula \\]" :color blue)
  ("q" nil "quit" :color blue))

(define-key hypertex-mode-map (kbd "s-f") 'hypertex-hydra/body)



(provide 'hypertex)

(defvar math-rewrite-for-display t)

(defun math-format-stack-value (entry)
  (message "formatting stack value %s" entry)
  (setq calc-selection-cache-entry calc-selection-cache-default-entry)
  (let* ((a (car entry))
	       (math-comp-selected (nth 2 entry))
	       (c (cond ((null a) "<nil>")
		              ((eq calc-display-raw t) (format "%s" a))
		              ((stringp a) a)
		              ((eq a 'top-of-stack) (propertize "." 'font-lock-face 'bold))
                  ;; This happens when you select part of a formula without changing it
		              (calc-prepared-composition
		               calc-prepared-composition)
		              ((and (Math-scalarp a)
			                  (memq calc-language '(nil flat unform))
			                  (null math-comp-selected))
		               (math-format-number a))
		              ((and math-rewrite-for-display
                        (not (string= calc-language "unform")))
                   (require 'calc-ext)
                   (let ((math-rewrite-for-display nil)
                         (rules '(var DispRules var-DispRules)))
                     (message "rewriting for display")
                     (math-compose-expr (math-rewrite (copy-tree a) rules) 0)))
                  (t (require 'calc-ext)
                     (math-compose-expr a 0))))
	       (off (math-stack-value-offset c))
	       s w)
    (and math-comp-selected (setq calc-any-selections t))
    (message "selected: %s" math-comp-selected)
    (message "comped: %s" c)
    (setq w (cdr off)
	        off (car off))
    (when (> off 0)
      (setq c (math-comp-concat (make-string off ?\s) c)))
    (or (equal calc-left-label "")
	      (setq c (math-comp-concat (if (eq a 'top-of-stack)
				                              (make-string (length calc-left-label) ?\s)
				                            calc-left-label)
				                          c)))
    (when calc-line-numbering
      (setq c (math-comp-concat (if (eq calc-language 'big)
				                            (if math-comp-selected
					                              '(tag t "1:  ")
				                              "1:  ")
				                          "    ")
				                        c)))
    (unless (or (equal calc-right-label "")
		            (eq a 'top-of-stack))
      (require 'calc-ext)
      (setq c (list 'horiz c
		                (make-string (max (- w (math-comp-width c)
					                               (length calc-right-label)) 0) ?\s)
		                '(break -1)
		                calc-right-label)))
    (setq s (if (stringp c)
		            (if calc-display-raw
		                (prin1-to-string c)
		              c)
	            (math-composition-to-string c w)))
    (when calc-language-output-filter
      (setq s (funcall calc-language-output-filter s)))
    (if (eq calc-language 'big)
	      (setq s (concat s "\n"))
      (when calc-line-numbering
	      (setq s (concat "1:" (substring s 2)))))
    (setcar (cdr entry) (calc-count-lines s))
    s))

(defun calc-prepare-selection (&optional num)
  (or num (setq num (calc-locate-cursor-element (point))))
  (setq calc-selection-true-num num
        calc-keep-selection t)
  (or (> num 0) (setq num 1))
  ;; (if (or (< num 1) (> num (calc-stack-size)))
  ;;     (error "Cursor must be positioned on a stack element"))
  (let* ((entry (calc-top num 'entry))
         ww w)
    (or (equal entry calc-selection-cache-entry)
        (progn
          (setcar entry (calc-encase-atoms (car entry)))
          (setq calc-selection-cache-entry entry
                calc-selection-cache-num num
                calc-selection-cache-comp
                (let* ((rules '(var DispRules var-DispRules))
                       (expr (if (and math-rewrite-for-display
                                      (not (string= calc-language "unform")))
                                 (math-rewrite (copy-tree (car entry)) rules)
                               (car entry)))
                       (math-comp-tagged t)
                       (math-rewrite-for-display nil))
                  (message "%s" expr)
                  (math-compose-expr expr 0))
                calc-selection-cache-offset
                (+ (car (math-stack-value-offset calc-selection-cache-comp))
                   (length calc-left-label)
                   (if calc-line-numbering 4 0))))))
  (calc-preserve-point))

(defun math-rewrite (math-rewrite-whole-expr rules &optional math-mt-many)
  (let* ((crules (math-compile-rewrites rules))
         (heads (math-rewrite-heads math-rewrite-whole-expr))
         (trace-buffer (get-buffer "*Trace*"))
         (calc-display-just 'center)
         (calc-display-origin 39)
         (calc-line-breaking 78)
         (calc-line-numbering nil)
         (calc-show-selections t)
         (calc-why nil)
         (math-mt-func (function
                        (lambda (x)
                          (let ((result (math-apply-rewrites x (cdr crules)
                                                             heads crules)))
                            (if result
                                (progn
                                  (if trace-buffer
                                      (let ((fmt (math-format-stack-value
                                                  (list result nil nil))))
                                        (with-current-buffer trace-buffer
                                          (insert "\nrewrite to\n" fmt "\n"))))
                                  (setq heads (math-rewrite-heads result heads t))))
                            result)))))
    (if trace-buffer
	      (let ((fmt (math-format-stack-value (list math-rewrite-whole-expr nil nil))))
	        (with-current-buffer trace-buffer
	          (setq truncate-lines t)
	          (goto-char (point-max))
	          (insert "\n\nBegin rewriting\n" fmt "\n"))))
    (or math-mt-many (setq math-mt-many (or (nth 1 (car crules))
				    math-rewrite-default-iters)))
    (if (equal math-mt-many '(var inf var-inf)) (setq math-mt-many 1000000))
    (if (equal math-mt-many '(neg (var inf var-inf))) (setq math-mt-many -1000000))
    (math-rewrite-phase (nth 3 (car crules)))
    (if trace-buffer
	      (let ((fmt (math-format-stack-value (list math-rewrite-whole-expr nil nil))))
	        (with-current-buffer trace-buffer
	          (insert "\nDone rewriting"
		                (if (= math-mt-many 0) " (reached iteration limit)" "")
		                ":\n" fmt "\n"))))
    math-rewrite-whole-expr))



(let* ((s calc-stack)
       (e (cdr s))
       (a (car (car e)))
       (b (nth 2 (car e))))
  (message "a: %s, b: %s, eq: %s" a b (eq a b)))
