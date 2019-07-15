(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calc-highlight-selections-with-faces t)
 '(cdlatex-command-alist
   (quote
    (("h" "Insert a hatted element" "\\hat{?}" cdlatex-position-cursor nil nil t)
     ("d," "Insert a differential element" "\\,d" ignore nil nil t))))
 '(cdlatex-math-symbol-alist (quote ((114 ("\\rho" "\\rcurs" "\\textbf{\\rcurs}")))))
 '(define-word-default-service (quote webster))
 '(ediff-keep-variants nil)
 '(fill-column 80)
 '(latex-run-command "xelatex")
 '(org-agenda-clock-consistency-checks
   (quote
    (:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                   ("4:00")
                   :default-face
                   ((:background "DarkRed")
                    (:foreground "white"))
                   :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil)))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 4)))
 '(org-agenda-custom-commands
   (quote
    ((" " "Agenda"
      ((agenda ""
               ((org-agenda-skip-function
                 (quote
                  (my-skip-tag "drill"))))))
      nil nil)
     ("o" "Organization"
      ((tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")))
       (tags-todo "-CANCELLED-WAIT-HOLD/!"
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function
                    (quote bh/skip-non-stuck-projects))
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (todo "WAIT"
             ((org-agenda-overriding-header "Blocked Tasks")))
       (todo "HOLD"
             ((org-agenda-overriding-header "On Hold"))))
      nil nil)
     ("t" "Tasks"
      ((todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (todo "TODO"
             ((org-agenda-overriding-header "TODOs"))))))))
 '(org-agenda-files
   (quote
    ("/ssh:emacs-node:/home/jack/org/calc.org" "/ssh:emacs-node:/home/jack/org/diary.org" "/ssh:emacs-node:/home/jack/org/electrodynamics.org" "/ssh:emacs-node:/home/jack/org/emacs.org" "/ssh:emacs-node:/home/jack/org/facts.org" "/ssh:emacs-node:/home/jack/org/journal.org" "/ssh:emacs-node:/home/jack/org/notes.org" "/ssh:emacs-node:/home/jack/org/personal.org" "/ssh:emacs-node:/home/jack/org/phone.org" "/ssh:emacs-node:/home/jack/org/reading.org" "/ssh:emacs-node:/home/jack/org/refile.org" "/ssh:emacs-node:/home/jack/org/spanish.org" "/ssh:emacs-node:/home/jack/org/work.org" "/ssh:emacs-node:/home/jack/org/workout.org")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled 1)
 '(org-agenda-window-setup (quote other-window))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (latex . t))))
 '(org-capture-templates
   (quote
    (("j" "Journal")
     ("jp" "Personal Journal" entry
      (file+olp+datetree "/ssh:emacs-node:/home/jack/org/journal.org")
      "* %?
%U
")
     ("jw" "Work Journal" entry
      (file+olp+datetree "/ssh:emacs-node:/home/jack/org/diary.org")
      "* %?
%U
" :jump-to-captured t :clock-in t :clock-resume t)
     ("m" "Meeting")
     ("mm" "Meeting" entry
      (file "/ssh:emacs-node:/home/jack/org/refile.org")
      "* MEETING with %? :MEETING:")
     ("ms" "Standup" entry
      (file+olp "/ssh:emacs-node:/home/jack/org/work.org" "Standups")
      "** MEETING %u :MEETING:standup:
%T" :clock-in t :clock-resume t)
     ("s" "Snippet")
     ("sv" "Contents of selection" entry
      (file "/ssh:emacs-node:/home/jack/org/refile.org")
      "* Snippet :snippet:
%i")
     ("t" "New task" entry
      (file "/ssh:emacs-node:/home/jack/org/refile.org")
      "* TODO %^{Task}" :clock-in t :clock-resume t)
     ("p" "Phone call" entry
      (file "/ssh:emacs-node:/home/jack/org/refile.org")
      "* Phone Call %? :PHONE:
%U" :clock-in t :clock-resume t)
     ("d" "Item to drill")
     ("dk" "New key binding" entry
      (file+olp "~/org/emacs.org" "Key Bindings")
      "*** Task :drill:
%^{Task}
**** Shortcut
%^{Shortcut}"))) t)
 '(org-clock-in-switch-to-state jack/org-clock-in-switch-state t)
 '(org-drill-left-cloze-delimiter "{[")
 '(org-drill-right-cloze-delimiter "]}")
 '(org-format-latex-header "%&~/.emacs.d/private/header
")
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-habit-show-habits-only-for-today t)
 '(org-latex-compiler "xelatex")
 '(org-latex-pdf-process
   (quote
    ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(org-modules (quote (org-docview org-habit org-info org-drill)))
 '(org-outline-path-complete-in-steps nil)
 '(org-preview-latex-default-process (quote dvisvgm))
 '(org-preview-latex-process-alist
   (quote
    ((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
     (dvisvgm :programs
              ("xelatex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("xelatex -no-pdf -interaction batchmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O")))))
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 9))))
 '(org-refile-use-outline-path (quote file))
 '(org-stuck-projects (quote ("" nil nil "")))
 '(org-tag-alist
   (quote
    (("work" . 119)
     ("personal" . 112)
     ("lizeth" . 108))))
 '(org-tags-exclude-from-inheritance (quote ("PROJECT")))
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "bisque")
     ("NEXT" . "gold")
     ("DONE" . "forest green")
     ("WAIT" . "chocolate")
     ("HOLD" . "light slate blue")
     ("CANCELLED" . "grey")
     ("PHONE" . "purple")
     ("MEETING" . "purple"))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w@/!)" "HOLD(h!/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
 '(package-selected-packages
   (quote
    (smeargle orgit magit-gitflow magit-popup gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit transient git-commit with-editor writeroom-mode olivetti outline-magic posframe dash-functional anki-editor rustic ht xterm-color web-mode tagedit slim-mode scss-mode sass-mode pug-mode haml-mode emmet-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby fuzzy company-statistics company-auctex company auto-yasnippet yasnippet ac-ispell auto-complete cdlatex org-projectile org-category-capture org-present org-pomodoro org-plus-contrib org-mime org-download org-bullets alert log4e gntp htmlize gnuplot auctex toml-mode racer pos-tip cargo markdown-mode rust-mode reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-make google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump popup f dash s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed async aggressive-indent adaptive-wrap ace-window ace-link avy)))
 '(send-mail-function (quote mailclient-send-it))
 '(split-height-threshold 120)
 '(split-width-threshold 100))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
