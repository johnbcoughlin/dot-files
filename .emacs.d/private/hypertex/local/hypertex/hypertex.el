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

(defvar math-trace-rewrites t)

(defun math-format-stack-value (entry)
  (message "formatting stack value")
  (setq calc-selection-cache-entry calc-selection-cache-default-entry)
  (let* ((a (car entry))
	       (math-comp-selected (nth 2 entry))
	       (c (cond ((null a) "<nil>")
		              ((eq calc-display-raw t) (format "%s" a))
		              ((stringp a) a)
		              ((eq a 'top-of-stack) (propertize "." 'font-lock-face 'bold))
		              (calc-prepared-composition
		               calc-prepared-composition)
		              ((and (Math-scalarp a)
			                  (memq calc-language '(nil flat unform))
			                  (null math-comp-selected))
		               (math-format-number a))
		              (t (require 'calc-ext)
                     (let ((math-trace-rewrites nil)
                           (rules '(var DispRules var-DispRules)))
                       (message "rewriting")
                       (math-compose-expr (math-rewrite a rules) 0)))))
	       (off (math-stack-value-offset c))
	       s w)
    (and math-comp-selected (setq calc-any-selections t))
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
                (let ((math-comp-tagged t)
                      (math-trace-rewrites nil)
                      (rules '(var DispRules var-DispRules)))
                  (message "%s" (car entry))
                  (message "%s" (math-rewrite (car entry) rules))
                  (math-compose-expr (math-rewrite (car entry) rules) 0))
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
                                  (if (and math-trace-rewrites trace-buffer)
                                      (let ((fmt (math-format-stack-value
                                                  (list result nil nil))))
                                        (with-current-buffer trace-buffer
                                          (insert "\nrewrite to\n" fmt "\n"))))
                                  (setq heads (math-rewrite-heads result heads t))))
                            result)))))
    (if (and math-trace-rewrites trace-buffer)
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
    (if (and math-trace-rewrites trace-buffer)
	      (let ((fmt (math-format-stack-value (list math-rewrite-whole-expr nil nil))))
	        (with-current-buffer trace-buffer
	          (insert "\nDone rewriting"
		                (if (= math-mt-many 0) " (reached iteration limit)" "")
		                ":\n" fmt "\n"))))
    math-rewrite-whole-expr))

;;; Give multiplication precedence when composing to avoid
;;; writing a*(b c) instead of a b c
(defun math-compose-expr (expr prec &optional div)
  (let ((calc-multiplication-has-precedence t)
        (math-compose-level (1+ math-compose-level))
        (math-expr-opers (math-expr-ops))
        (a (let ((math-trace-rewrites nil)
                 (rules '(var DispRules var-DispRules)))
             (math-rewrite a rules)))
        spfn)
    (cond
     ((or (and (eq a math-comp-selected) a)
	  (and math-comp-tagged
	       (not (eq math-comp-tagged a))))
      (let ((math-comp-selected nil))
	(and math-comp-tagged (setq math-comp-tagged a))
	(list 'tag a (math-compose-expr a prec))))
     ((and (not (consp a)) (not (integerp a)))
      (concat "'" (prin1-to-string a)))
     ((setq spfn (assq (car-safe a)
                       (get calc-language 'math-special-function-table)))
      (setq spfn (cdr spfn))
      (if (consp spfn)
          (funcall (car spfn) a spfn)
        (funcall spfn a)))
     ((math-scalarp a)
      (if (or (eq (car-safe a) 'frac)
	      (and (nth 1 calc-frac-format) (Math-integerp a)))
	  (if (and
               calc-language
               (not (memq calc-language
                          '(flat big unform))))
	      (let ((aa (math-adjust-fraction a))
		    (calc-frac-format nil))
		(math-compose-expr (list '/
					 (if (memq calc-language
                                                   calc-lang-slash-idiv)
					     (math-float (nth 1 aa))
					   (nth 1 aa))
					 (nth 2 aa)) prec))
	    (if (and (eq calc-language 'big)
		     (= (length (car calc-frac-format)) 1))
		(let* ((aa (math-adjust-fraction a))
		       (calc-frac-format nil)
		       (math-radix-explicit-format nil)
		       (c (list 'horiz
				(if (math-negp (nth 1 aa))
				    "- " "")
				(list 'vcent 1
				      (math-format-number
				       (math-abs (nth 1 aa)))
				      '(rule ?-)
				      (math-format-number (nth 2 aa))))))
		  (if (= calc-number-radix 10)
		      c
		    (list 'horiz "(" c
			  (list 'subscr ")"
				(int-to-string calc-number-radix)))))
	      (math-format-number a)))
	(if (not (eq calc-language 'big))
	    (math-format-number a prec)
	  (if (memq (car-safe a) '(cplx polar))
	      (if (math-zerop (nth 2 a))
		  (math-compose-expr (nth 1 a) prec)
		(list 'horiz "("
		      (math-compose-expr (nth 1 a) 0)
		      (if (eq (car a) 'cplx) ", " "; ")
		      (math-compose-expr (nth 2 a) 0) ")"))
	    (if (or (= calc-number-radix 10)
		    (not (Math-realp a))
		    (and calc-group-digits
			 (not (assoc calc-group-char '((",") (" "))))))
		(math-format-number a prec)
	      (let ((s (math-format-number a prec))
		    (c nil))
		(while (string-match (if (> calc-number-radix 14)
					 "\\([0-9]+\\)#\\([0-9a-zA-Z., ]+\\)"
				       "\\([0-9]+\\)#\\([0-9a-dA-D., ]+\\)")
				     s)
		  (setq c (nconc c (list (substring s 0 (match-beginning 0))
					 (list 'subscr
					       (math-match-substring s 2)
					       (math-match-substring s 1))))
			s (substring s (match-end 0))))
		(if (string-match
		     "\\*\\([0-9.]+\\)\\^\\(-?[0-9]+\\)\\()?\\)\\'" s)
		    (setq s (list 'horiz
				  (substring s 0 (match-beginning 0)) " "
				  (list 'supscr
					(math-match-substring s 1)
					(math-match-substring s 2))
				  (math-match-substring s 3))))
		(if c (cons 'horiz (nconc c (list s))) s)))))))
     ((and (get (car a) 'math-compose-forms)
	   (not (eq calc-language 'unform))
	   (let ((comps (get (car a) 'math-compose-forms))
		 temp temp2)
	     (or (and (setq temp (assq calc-language comps))
		      (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			       (setq temp (apply (cdr temp2) (cdr a)))
			       (math-compose-expr temp prec))
			  (and (setq temp2 (assq nil (cdr temp)))
			       (funcall (cdr temp2) a))))
		 (and (setq temp (assq nil comps))
		      (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			       (setq temp (apply (cdr temp2) (cdr a)))
			       (math-compose-expr temp prec))
			  (and (setq temp2 (assq nil (cdr temp)))
			       (funcall (cdr temp2) a))))))))
     ((eq (car a) 'vec)
      (let* ((math-comp-left-bracket (if calc-vector-brackets
			       (substring calc-vector-brackets 0 1) ""))
	     (math-comp-right-bracket (if calc-vector-brackets
				(substring calc-vector-brackets 1 2) ""))
	     (inner-brackets (memq 'R calc-matrix-brackets))
	     (outer-brackets (memq 'O calc-matrix-brackets))
	     (row-commas (memq 'C calc-matrix-brackets))
	     (math-comp-comma-spc (or calc-vector-commas " "))
	     (math-comp-comma (or calc-vector-commas ""))
	     (math-comp-vector-prec (if (or (and calc-vector-commas
				       (math-vector-no-parens a))
				  (memq 'P calc-matrix-brackets)) 0 1000))
	     (math-comp-just (cond ((eq calc-matrix-just 'right) 'vright)
                                      ((eq calc-matrix-just 'center) 'vcent)
                                      (t 'vleft)))
	     (break calc-break-vectors))
	(if (and (memq calc-language '(nil big))
		 (not calc-break-vectors)
		 (math-matrixp a) (not (math-matrixp (nth 1 a)))
		 (or calc-full-vectors
		     (and (< (length a) 7) (< (length (nth 1 a)) 7))
		     (progn (setq break t) nil)))
	    (if (progn
		  (setq math-comp-vector-prec (if (or (and calc-vector-commas
                                                           (math-vector-no-parens
                                                            (nth 1 a)))
                                                      (memq 'P calc-matrix-brackets))
                                                  0 1000))
		  (= (length a) 2))
		(list 'horiz
		      (concat math-comp-left-bracket math-comp-left-bracket " ")
		      (math-compose-vector (cdr (nth 1 a)) (concat math-comp-comma " ")
					   math-comp-vector-prec)
		      (concat " " math-comp-right-bracket math-comp-right-bracket))
	      (let* ((rows (1- (length a)))
		     (cols (1- (length (nth 1 a))))
		     (base (/ (1- rows) 2))
		     (calc-language 'flat))
		(append '(horiz)
			(list (append '(vleft)
				      (list base)
				      (list (concat (and outer-brackets
							 (concat math-comp-left-bracket
								 " "))
						    (and inner-brackets
							 (concat math-comp-left-bracket
								 " "))))
				      (make-list (1- rows)
						 (concat (and outer-brackets
							      "  ")
							 (and inner-brackets
							      (concat
							       math-comp-left-bracket
							       " "))))))
			(math-compose-matrix (cdr a) 1 cols base)
			(list (append '(vleft)
				      (list base)
				      (make-list (1- rows)
						 (if inner-brackets
						     (concat " "
							     math-comp-right-bracket
							     (and row-commas
								  math-comp-comma))
						   (if (and outer-brackets
							    row-commas)
						       ";" "")))
				      (list (concat
					     (and inner-brackets
						  (concat " "
							  math-comp-right-bracket))
					     (and outer-brackets
						  (concat
						   " "
						   math-comp-right-bracket)))))))))
	  (if (and calc-display-strings
		   (cdr a)
		   (math-vector-is-string a))
	      (math-vector-to-string a t)
	    (if (and break (cdr a)
		     (not (eq calc-language 'flat)))
		(let* ((full (or calc-full-vectors (< (length a) 7)))
		       (rows (if full (1- (length a)) 5))
		       (base (/ (1- rows) 2))
		       (calc-break-vectors nil))
		  (list 'horiz
			(cons 'vleft (cons base
					   (math-compose-rows
					    (cdr a)
					    (if full rows 3) t)))))
	      (if (or calc-full-vectors (< (length a) 7))
                  (if (and
                       (setq spfn (get calc-language 'math-matrix-formatter))
                       (math-matrixp a))
                      (funcall spfn a)
                    (list 'horiz
                          math-comp-left-bracket
                          (math-compose-vector (cdr a)
                                               (concat math-comp-comma " ")
                                               math-comp-vector-prec)
                          math-comp-right-bracket))
		(list 'horiz
		      math-comp-left-bracket
		      (math-compose-vector (list (nth 1 a) (nth 2 a) (nth 3 a))
					   (concat math-comp-comma " ")
                                           math-comp-vector-prec)
		      math-comp-comma
                      (if (setq spfn (get calc-language 'math-dots))
                          (concat " " spfn)
                        " ...")
		      math-comp-comma " "
		      (list 'break math-compose-level)
		      (math-compose-expr (nth (1- (length a)) a)
					 (if (equal math-comp-comma "") 1000 0))
		      math-comp-right-bracket)))))))
     ((eq (car a) 'incomplete)
      (if (cdr (cdr a))
	  (cond ((eq (nth 1 a) 'vec)
		 (list 'horiz "["
		       (math-compose-vector (cdr (cdr a)) ", " 0)
		       " ..."))
		((eq (nth 1 a) 'cplx)
		 (list 'horiz "("
		       (math-compose-vector (cdr (cdr a)) ", " 0)
		       ", ..."))
		((eq (nth 1 a) 'polar)
		 (list 'horiz "("
		       (math-compose-vector (cdr (cdr a)) "; " 0)
		       "; ..."))
		((eq (nth 1 a) 'intv)
		 (list 'horiz
		       (if (memq (nth 2 a) '(0 1)) "(" "[")
		       (math-compose-vector (cdr (cdr (cdr a))) " .. " 0)
		       " .. ..."))
		(t (format "%s" a)))
	(cond ((eq (nth 1 a) 'vec) "[ ...")
	      ((eq (nth 1 a) 'intv)
	       (if (memq (nth 2 a) '(0 1)) "( ..." "[ ..."))
	      (t "( ..."))))
     ((eq (car a) 'var)
      (let ((v (rassq (nth 2 a) math-expr-variable-mapping)))
	(if v
	    (symbol-name (car v))
          (if (setq spfn (get calc-language 'math-var-formatter))
              (funcall spfn a prec)
            (math-compose-var a)))))
     ((eq (car a) 'intv)
      (list 'horiz
            (if (memq (nth 1 a) '(0 1)) "(" "[")
	    (math-compose-expr (nth 2 a) 0)
            " .. "
	    (math-compose-expr (nth 3 a) 0)
            (if (memq (nth 1 a) '(0 2)) ")" "]")))
     ((eq (car a) 'date)
      (if (eq (car calc-date-format) 'X)
	  (math-format-date a)
	(concat "<" (math-format-date a) ">")))
     ((and (eq (car a) 'calcFunc-subscr)
           (setq spfn (get calc-language 'math-compose-subscr)))
      (funcall spfn a))
     ((and (eq (car a) 'calcFunc-subscr) (= (length a) 3)
	   (eq calc-language 'big))
      (let* ((a1 (math-compose-expr (nth 1 a) 1000))
	     (calc-language 'flat)
	     (a2 (math-compose-expr (nth 2 a) 0)))
	(if (or (eq (car-safe a1) 'subscr)
		(and (eq (car-safe a1) 'tag)
		     (eq (car-safe (nth 2 a1)) 'subscr)
		     (setq a1 (nth 2 a1))))
	    (list 'subscr
		  (nth 1 a1)
		  (list 'horiz
			(nth 2 a1)
			", "
			a2))
	  (list 'subscr a1 a2))))
     ((and (eq (car a) '^)
	   (eq calc-language 'big))
      (list 'supscr
	    (if (or (math-looks-negp (nth 1 a))
		    (memq (car-safe (nth 1 a)) '(^ / frac calcFunc-sqrt))
		    (and (eq (car-safe (nth 1 a)) 'cplx)
			 (math-negp (nth 1 (nth 1 a)))
			 (eq (nth 2 (nth 1 a)) 0)))
		(list 'horiz "(" (math-compose-expr (nth 1 a) 0) ")")
	      (math-compose-expr (nth 1 a) 201))
	    (let ((calc-language 'flat)
		  (calc-number-radix 10)
                  (calc-twos-complement-mode nil))
	      (math-compose-expr (nth 2 a) 0))))
     ((and (eq (car a) '/)
	   (eq calc-language 'big))
      (let ((a1 (let ((calc-language (if (memq (car-safe (nth 1 a)) '(/ frac))
					 'flat 'big)))
		  (math-compose-expr (nth 1 a) 0)))
	    (a2 (let ((calc-language (if (memq (car-safe (nth 2 a)) '(/ frac))
					 'flat 'big)))
		  (math-compose-expr (nth 2 a) 0))))
	(list 'vcent
	      (math-comp-height a1)
	      a1 '(rule ?-) a2)))
     ((and (eq (car a) 'calcFunc-lambda)
	   (> (length a) 2)
	   (memq calc-language '(nil flat big)))
      (let ((p (cdr a))
	    (ap calc-arg-values)
	    (math-compose-hash-args (if (= (length a) 3) 1 t)))
	(while (and (cdr p) (equal (car p) (car ap)))
	  (setq p (cdr p) ap (cdr ap)))
	(append '(horiz "<")
		(if (cdr p)
		    (list (math-compose-vector
			   (nreverse (cdr (reverse (cdr a)))) ", " 0)
			  " : ")
		  nil)
		(list (math-compose-expr (nth (1- (length a)) a) 0)
		      ">"))))
     ((and (eq (car a) 'calcFunc-string)
	   (= (length a) 2)
	   (math-vectorp (nth 1 a))
	   (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	  (concat "string(" (math-vector-to-string (nth 1 a) t) ")")
	(math-vector-to-string (nth 1 a) nil)))
     ((and (eq (car a) 'calcFunc-bstring)
	   (= (length a) 2)
	   (math-vectorp (nth 1 a))
	   (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	  (concat "bstring(" (math-vector-to-string (nth 1 a) t) ")")
	(let ((c nil)
	      (s (math-vector-to-string (nth 1 a) nil))
	      p)
	  (while (string-match "[^ ] +[^ ]" s)
	    (setq p (1- (match-end 0))
		  c (cons (list 'break math-compose-level)
			  (cons (substring s 0 p)
				c))
		  s (substring s p)))
	  (setq c (nreverse (cons s c)))
	  (or (= prec -123)
	      (setq c (cons (list 'set math-compose-level 2) c)))
	  (cons 'horiz c))))
     ((and (eq (car a) 'calcFunc-cprec)
	   (not (eq calc-language 'unform))
	   (= (length a) 3)
	   (integerp (nth 2 a)))
      (let ((c (math-compose-expr (nth 1 a) -1)))
	(if (> prec (nth 2 a))
            (if (setq spfn (get calc-language 'math-big-parens))
                (list 'horiz (car spfn) c (cdr spfn))
              (list 'horiz "(" c ")"))
	  c)))
     ((and (eq (car a) 'calcFunc-choriz)
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3 4))
	   (math-vectorp (nth 1 a))
	   (if (integerp (nth 2 a))
	       (or (null (nth 3 a))
		   (and (math-vectorp (nth 3 a))
			(math-vector-is-string (nth 3 a))))
	     (or (null (nth 2 a))
		 (and (math-vectorp (nth 2 a))
		      (math-vector-is-string (nth 2 a))))))
      (let* ((cprec (and (integerp (nth 2 a)) (nth 2 a)))
	     (sep (nth (if cprec 3 2) a))
	     (bprec nil))
	(if sep
	    (math-compose-vector (cdr (nth 1 a))
				 (math-vector-to-string sep nil)
				 (or cprec prec))
	  (cons 'horiz (mapcar (function
				(lambda (x)
				  (if (eq (car-safe x) 'calcFunc-bstring)
				      (prog1
					  (math-compose-expr
					   x (or bprec cprec prec))
					(setq bprec -123))
				    (math-compose-expr x (or cprec prec)))))
			       (cdr (nth 1 a)))))))
     ((and (memq (car a) '(calcFunc-cvert calcFunc-clvert calcFunc-crvert))
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3))
	   (math-vectorp (nth 1 a))
	   (or (null (nth 2 a))
	       (integerp (nth 2 a))))
      (let* ((base 0)
	     (v 0)
	     (prec (or (nth 2 a) prec))
	     (c (mapcar (function
			 (lambda (x)
			   (let ((b nil) (cc nil) a d)
			     (if (and (memq (car-safe x) '(calcFunc-cbase
							   calcFunc-ctbase
							   calcFunc-cbbase))
				      (memq (length x) '(1 2)))
				 (setq b (car x)
				       x (nth 1 x)))
			     (if (and (eq (car-safe x) 'calcFunc-crule)
				      (memq (length x) '(1 2))
				      (or (null (nth 1 x))
					  (and (math-vectorp (nth 1 x))
					       (= (length (nth 1 x)) 2)
					       (math-vector-is-string
						(nth 1 x)))
					  (and (natnump (nth 1 x))
					       (<= (nth 1 x) 255))))
				 (setq cc (list
					   'rule
					   (if (math-vectorp (nth 1 x))
					       (aref (math-vector-to-string
						      (nth 1 x) nil) 0)
					     (or (nth 1 x) ?-))))
			       (or (and (memq (car-safe x) '(calcFunc-cvspace
							     calcFunc-ctspace
							     calcFunc-cbspace))
					(memq (length x) '(2 3))
					(eq (nth 1 x) 0))
				   (null x)
				   (setq cc (math-compose-expr x prec))))
			     (setq a (if cc (math-comp-ascent cc) 0)
				   d (if cc (math-comp-descent cc) 0))
			     (if (eq b 'calcFunc-cbase)
				 (setq base (+ v a -1))
			       (if (eq b 'calcFunc-ctbase)
				   (setq base v)
				 (if (eq b 'calcFunc-cbbase)
				     (setq base (+ v a d -1)))))
			     (setq v (+ v a d))
			     cc)))
			(cdr (nth 1 a)))))
	(setq c (delq nil c))
	(if c
	    (cons (if (eq (car a) 'calcFunc-cvert) 'vcent
		    (if (eq (car a) 'calcFunc-clvert) 'vleft 'vright))
		  (cons base c))
	  " ")))
     ((and (memq (car a) '(calcFunc-csup calcFunc-csub))
	   (not (eq calc-language 'unform))
	   (memq (length a) '(3 4))
	   (or (null (nth 3 a))
	       (integerp (nth 3 a))))
      (list (if (eq (car a) 'calcFunc-csup) 'supscr 'subscr)
	    (math-compose-expr (nth 1 a) (or (nth 3 a) 0))
	    (math-compose-expr (nth 2 a) 0)))
     ((and (eq (car a) 'calcFunc-cflat)
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3))
	   (or (null (nth 2 a))
	       (integerp (nth 2 a))))
      (let ((calc-language (if (memq calc-language '(nil big))
			       'flat calc-language)))
	(math-compose-expr (nth 1 a) (or (nth 2 a) 0))))
     ((and (eq (car a) 'calcFunc-cspace)
	   (memq (length a) '(2 3))
	   (natnump (nth 1 a)))
      (if (nth 2 a)
	  (cons 'horiz (make-list (nth 1 a)
				  (if (and (math-vectorp (nth 2 a))
					   (math-vector-is-string (nth 2 a)))
				      (math-vector-to-string (nth 2 a) nil)
				    (math-compose-expr (nth 2 a) 0))))
	(make-string (nth 1 a) ?\ )))
     ((and (memq (car a) '(calcFunc-cvspace calcFunc-ctspace calcFunc-cbspace))
	   (memq (length a) '(2 3))
	   (natnump (nth 1 a)))
      (if (= (nth 1 a) 0)
	  ""
	(let* ((c (if (nth 2 a)
		      (if (and (math-vectorp (nth 2 a))
			       (math-vector-is-string (nth 2 a)))
			  (math-vector-to-string (nth 2 a) nil)
			(math-compose-expr (nth 2 a) 0))
		    " "))
	       (ca (math-comp-ascent c))
	       (cd (math-comp-descent c)))
	  (cons 'vleft
		(cons (if (eq (car a) 'calcFunc-ctspace)
			  (1- ca)
			(if (eq (car a) 'calcFunc-cbspace)
			    (+ (* (1- (nth 1 a)) (+ ca cd)) (1- ca))
			  (/ (1- (* (nth 1 a) (+ ca cd))) 2)))
		      (make-list (nth 1 a) c))))))
     ((and (eq (car a) 'calcFunc-evalto)
	   (setq calc-any-evaltos t)
	   (setq spfn (get calc-language 'math-evalto))
	   (= math-compose-level (if math-comp-tagged 2 1))
	   (= (length a) 3))
      (list 'horiz
            (car spfn)
	    (math-compose-expr (nth 1 a) 0)
	    (cdr spfn)
	    (math-compose-expr (nth 2 a) 0)))
     (t
      (let ((op (and (not (eq calc-language 'unform))
		     (if (and (eq (car a) 'calcFunc-if) (= (length a) 4))
			 (assoc "?" math-expr-opers)
		       (math-assq2 (car a) math-expr-opers)))))
	(cond ((and op
		    (or (= (length a) 3) (eq (car a) 'calcFunc-if))
		    (/= (nth 3 op) -1))
	       (cond
		((or
                  (> prec (or (nth 4 op) (min (nth 2 op) (nth 3 op))))
                  (and div (eq (car a) '*)))
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (if (eq (car-safe a) '/)
			 (list 'horiz "{" (math-compose-expr a -1) "}")
		       (list 'horiz "\\left( "
			     (math-compose-expr a -1)
			     " \\right)"))
		   (if (eq calc-language 'eqn)
		       (if (or (eq (car-safe a) '/)
			       (= (/ prec 100) 9))
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "( " (math-compose-expr a -1) " )")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (list 'horiz "(" (math-compose-expr a 0) ")"))))
		((and (memq calc-language '(tex latex))
		      (memq (car a) '(/ calcFunc-choose calcFunc-evalto))
		      (>= prec 0))
		 (list 'horiz "{" (math-compose-expr a -1) "}"))
		((eq (car a) 'calcFunc-if)
		 (list 'horiz
		       (math-compose-expr (nth 1 a) (nth 2 op))
		       " ? "
		       (math-compose-expr (nth 2 a) 0)
		       " : "
		       (math-compose-expr (nth 3 a) (nth 3 op))))
		(t
		 (let* ((math-comp-tagged (and math-comp-tagged
					       (not (math-primp a))
					       math-comp-tagged))
			(setlev (if (= prec (min (nth 2 op) (nth 3 op)))
				    (progn
				      (setq math-compose-level
					    (1- math-compose-level))
				      nil)
				  math-compose-level))
			(lhs (math-compose-expr (nth 1 a) (nth 2 op)))
			(rhs (math-compose-expr (nth 2 a) (nth 3 op) (eq (nth 1 op) '/))))
		   (and (equal (car op) "^")
			(eq (math-comp-first-char lhs) ?-)
			(setq lhs (list 'horiz "(" lhs ")")))
		   (and (memq calc-language '(tex latex))
			(or (equal (car op) "^") (equal (car op) "_"))
			(not (and (stringp rhs) (= (length rhs) 1)))
			(setq rhs (list 'horiz "{" rhs "}")))
		   (or (and (eq (car a) '*)
			    (or (null calc-language)
				(assoc "2x" math-expr-opers))
			    (let* ((prevt (math-prod-last-term (nth 1 a)))
				   (nextt (math-prod-first-term (nth 2 a)))
				   (prevc (or (math-comp-last-char lhs)
					      (and (memq (car-safe prevt)
							 '(^ calcFunc-subscr
							     calcFunc-sqrt
							     frac))
						   (eq calc-language 'big)
						   ?0)))
				   (nextc (or (math-comp-first-char rhs)
					      (and (memq (car-safe nextt)
							 '(calcFunc-sqrt
							   calcFunc-sum
							   calcFunc-prod
							   calcFunc-integ))
						   (eq calc-language 'big)
						   ?0))))
			      (and prevc nextc
				   (or (and (>= nextc ?a) (<= nextc ?z))
				       (and (>= nextc ?A) (<= nextc ?Z))
				       (and (>= nextc ?α) (<= nextc ?ω))
				       (and (>= nextc ?Α) (<= nextc ?Ω))
				       (and (>= nextc ?0) (<= nextc ?9))
				       (memq nextc '(?. ?_ ?#
							?\( ?\[ ?\{))
				       (and (eq nextc ?\\)
					    (not (string-match
						  "\\`\\\\left("
						  (math-comp-first-string
						   rhs)))))
				   (not (and (eq (car-safe prevt) 'var)
					     (eq nextc ?\()))
				   (list 'horiz
					 (list 'set setlev 1)
					 lhs
					 (list 'break math-compose-level)
					 " "
					 rhs))))
		       (list 'horiz
			     (list 'set setlev 1)
			     lhs
			     (list 'break math-compose-level)
			     (if (or (equal (car op) "^")
				     (equal (car op) "_")
				     (equal (car op) "**")
				     (and (equal (car op) "*")
					  (math-comp-last-char lhs)
					  (math-comp-first-char rhs))
				     (and (equal (car op) "/")
					  (math-num-integerp (nth 1 a))
					  (math-integerp (nth 2 a))))
				 (car op)
			       (if (and (eq calc-language 'big)
					(equal (car op) "=>"))
				   "  =>  "
				 (concat " " (car op) " ")))
			     rhs))))))
	      ((and op (= (length a) 2) (= (nth 3 op) -1))
	       (cond
		((or (> prec (or (nth 4 op) (nth 2 op)))
		     (and (not (eq (assoc (car op) math-expr-opers) op))
			  (> prec 0)))   ; don't write x% + y
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (list 'horiz "\\left( "
			   (math-compose-expr a -1)
			   " \\right)")
		   (if (eq calc-language 'eqn)
		       (if (= (/ prec 100) 9)
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "{( " (math-compose-expr a -1) " )}")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (list 'horiz "(" (math-compose-expr a 0) ")"))))
		(t
		 (let ((lhs (math-compose-expr (nth 1 a) (nth 2 op))))
		 (list 'horiz
		       lhs
		       (if (or (> (length (car op)) 1)
			       (not (math-comp-is-flat lhs)))
			   (concat " " (car op))
			 (car op)))))))
	      ((and op (= (length a) 2) (= (nth 2 op) -1))
	       (cond
		((eq (nth 3 op) 0)
		 (let ((lr (and (memq calc-language '(tex latex))
				(not (math-tex-expr-is-flat (nth 1 a))))))
		   (list 'horiz
			 (if lr "\\left" "")
			 (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'" (car op))
			     (substring (car op) 1)
			   (car op))
			 (if (or lr (> (length (car op)) 2)) " " "")
			 (math-compose-expr (nth 1 a) -1)
			 (if (or lr (> (length (car op)) 2)) " " "")
			 (if lr "\\right" "")
			 (car (nth 1 (memq op math-expr-opers))))))
		((> prec (or (nth 4 op) (nth 3 op)))
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (list 'horiz "\\left( "
			   (math-compose-expr a -1)
			   " \\right)")
		   (if (eq calc-language 'eqn)
		       (if (= (/ prec 100) 9)
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "{( " (math-compose-expr a -1) " )}")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (list 'horiz "(" (math-compose-expr a 0) ")"))))
		(t
		 (let ((rhs (math-compose-expr (nth 1 a) (nth 3 op))))
		   (list 'horiz
			 (let ((ops (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'"
						      (car op))
					(substring (car op) 1)
				      (car op))))
			   (if (or (> (length ops) 1)
				   (not (math-comp-is-flat rhs)))
			       (concat ops " ")
			     ops))
			 rhs)))))
	      ((and (eq calc-language 'big)
		    (setq op (get (car a) 'math-compose-big))
		    (funcall op a prec)))
	      ((and (setq op (assq calc-language
				   '( ( nil . math-compose-normal )
				      ( flat . math-compose-normal )
				      ( big . math-compose-normal )
				      ( c . math-compose-c )
				      ( pascal . math-compose-pascal )
				      ( fortran . math-compose-fortran )
				      ( tex . math-compose-tex )
				      ( latex . math-compose-latex )
				      ( eqn . math-compose-eqn )
                                      ( yacas . math-compose-yacas )
                                      ( maxima . math-compose-maxima )
                                      ( giac . math-compose-giac )
				      ( math . math-compose-math )
				      ( maple . math-compose-maple ))))
		    (setq op (get (car a) (cdr op)))
		    (funcall op a prec)))
	      (t
	       (let* ((func (car a))
		      (func2 (assq func '(( mod . calcFunc-makemod )
					  ( sdev . calcFunc-sdev )
					  ( + . calcFunc-add )
					  ( - . calcFunc-sub )
					  ( * . calcFunc-mul )
					  ( / . calcFunc-div )
					  ( % . calcFunc-mod )
					  ( ^ . calcFunc-pow )
					  ( neg . calcFunc-neg )
					  ( | . calcFunc-vconcat ))))
		      left right args)
		 (if func2
		     (setq func (cdr func2)))
		 (if (setq func2 (rassq func math-expr-function-mapping))
		     (setq func (car func2)))
		 (setq func (math-remove-dashes
			     (if (string-match
				  "\\`calcFunc-\\([a-zA-Zα-ωΑ-Ω0-9']+\\)\\'"
				  (symbol-name func))
				 (math-match-substring (symbol-name func) 1)
			       (symbol-name func))))
		 (if (memq calc-language calc-lang-allow-percentsigns)
		     (setq func (math-to-percentsigns func)))
		 (if (memq calc-language calc-lang-allow-underscores)
		     (setq func (math-to-underscores func)))
                 (if (setq spfn (get calc-language 'math-func-formatter))
                     (funcall spfn func a)
                   (list 'horiz func calc-function-open
		       (math-compose-vector (cdr a) ", " 0)
		       calc-function-close))))))))))
