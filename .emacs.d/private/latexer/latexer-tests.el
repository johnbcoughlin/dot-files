(require 'hypertex)
(require 'ert)

(defun hyper-latex-commands-on-str (string keys)
  "Insert string in tmp buffer, execute keys and return the result"
  (let ((win (selected-window)))
    (unwind-protect
        (with-temp-buffer
          (set-window-buffer win (current-buffer) t)
          (turn-on-evil-mode)
          (turn-on-hyper-latex-mode)
          (insert string)
          (goto-char (point-min))
          (execute-kbd-macro keys)
          (buffer-string))
      (set-window-buffer win (current-buffer) t))))

(defun hyper-latex-execute-on-str (string point body)
  "Insert STRING into a temp buffer, execute BODY, and return the result"
  (let ((win (selected-window)))
    (unwind-protect
        (with-temp-buffer
          (set-window-buffer win (current-buffer) t)
          (turn-on-evil-mode)
          (turn-on-hypertex-mode)
          (insert string)
          (goto-char point)
          (funcall body))
      (set-window-buffer win (current-buffer) t))))

(ert-deftest evil-hypertex-atom-forward-test ()
  (should (equal (hyper-latex-execute-on-str
                  "stuff
\\[
\\cos\\theta
\\]"
                  11
                  (lambda () (progn
                               (evil-hypertex-atom-forward 1)
                               (point))))
                 14)))

(ert-deftest hyper-latex--subscript-no-brackets ()
  (should (equal (hyper-latex-execute-on-str
                  "_123 "
                  2
                  (lambda () (hyper-latex--subscript -1 nil 3 3)))
                 (list 2 5 :expanded t))))

(ert-deftest hyper-latex--subscript-brackets ()
  (should (equal (hyper-latex-execute-on-str
                  "_{123} "
                  3
                  (lambda () (hyper-latex--subscript -1 nil 3 3)))
                 (list 3 6 :expanded t)))
  (should (equal (hyper-latex-execute-on-str
                  "_{123} "
                  3
                  (lambda () (hyper-latex--subscript 1 t 3 3)))
                 (list 1 7 nil)))
  )

(ert-deftest evil-select-paren-test ()
  (let
      ((subject (lambda (count beg end)
                 (hyper-latex-execute-on-str
                                 "_{123} "
                                 3
                                 (lambda () (evil-select-paren ?{ ?} beg end nil count nil))))))
    (progn
      (should (equal (funcall subject 1 3 3)
                     (list 3 6 :expanded t)))
      (should (equal (funcall subject 1 3 5)
                     (list 3 6 :expanded t)))
      (should (equal (funcall subject -1 3 3)
                     (list 3 6 :expanded t)))
      )))

(ert-deftest evil-select-regexp-test ()
  (let
      ((subject (lambda (str point count beg end)
                  (hyper-latex-execute-on-str
                   str
                   point
                   (lambda () (evil-select-paren "_" "\\s-" beg end nil count nil))))))
    (progn
      (should (equal (funcall subject "_123 abc " 2 1 3 3)
                     (list 2 5 :expanded t)))
      (should (equal (funcall subject "_123^abc " 2 1 3 3)
                     (list 2 5 :expanded t)))
      )))
