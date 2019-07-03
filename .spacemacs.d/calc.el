;;; Definition stored by Calc on Fri Jun 14 22:10:21 2019
(put 'calc-define 'calcFunc-mysuperscript '(progn
 (defun calcFunc-mysuperscript (x) (math-check-const x t)
  (math-normalize x))
 (put 'calcFunc-mysuperscript 'calc-user-defn '(var x var-x))
 (put 'calcFunc-mysuperscript 'math-compose-forms '((latex (1 lambda
  (val) (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 94 123))) val (list (quote
  calcFunc-string) (quote (vec 125)))))))))
))

;;; Definition stored by Calc on Fri Jun 14 22:11:53 2019
(put 'calc-define 'calcFunc-mysuperscript '(progn
 (defun calcFunc-mysuperscript (x) (math-check-const x t)
  (math-normalize x))
 (put 'calcFunc-mysuperscript 'calc-user-defn '(var x var-x))
 (put 'calcFunc-mysuperscript 'math-compose-forms '((nil (1 lambda (x)
  (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 94 123))) x (list (quote calcFunc-string)
  (quote (vec 125))))))) (latex (1 lambda (val) (list (quote
  calcFunc-choriz) (list (quote vec) (list (quote calcFunc-string)
  (quote (vec 94 123))) val (list (quote calcFunc-string) (quote (vec
  125)))))))))
))

;;; Definition stored by Calc on Fri Jun 14 22:27:23 2019
(put 'calc-define 'calcFunc-deldel '(progn
 (defun calcFunc-deldel (var) (math-check-const var t) (math-normalize
  (list (quote calcFunc-lambda) (quote (var T var-T)) (list (quote
  calcFunc-deriv) (quote (var T var-T)) var))))
 (put 'calcFunc-deldel 'calc-user-defn '(calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var var var-var))))
 (put 'calcFunc-deldel 'math-compose-forms '((nil (1 lambda (var) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 112 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125))))))) (latex (1 lambda
  (var) (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 112 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125)))))))))
))

;;; Definition stored by Calc on Fri Jun 14 22:27:27 2019
(put 'calc-define 'calcFunc-deedee '(progn
 (defun calcFunc-deedee (var) (math-check-const var t) (math-normalize
  (list (quote calcFunc-lambda) (quote (var T var-T)) (list (quote
  calcFunc-deriv) (quote (var T var-T)) var))))
 (put 'calcFunc-deedee 'calc-user-defn '(calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var var var-var))))
 (put 'calcFunc-deedee 'math-compose-forms '((nil (1 lambda (var) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 109 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125))))))) (latex (1 lambda
  (var) (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 109 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125)))))))))
))

;;; Definition stored by Calc on Sat Jun 15 12:57:58 2019
(put 'calc-define 'calcFunc-deedee '(progn
 (defun calcFunc-deedee (var) (math-check-const var t) (math-normalize
  (list (quote calcFunc-diffop) (list (quote calcFunc-lambda) (quote
  (var T var-T)) (list (quote calcFunc-deriv) (quote (var T var-T))
  var)))))
 (put 'calcFunc-deedee 'calc-user-defn '(calcFunc-diffop (calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var var var-var)))))
 (put 'calcFunc-deedee 'math-compose-forms '((nil (1 lambda (var) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 109 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125))))))) (latex (1 lambda
  (var) (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 109 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125)))))))))
))

;;; Definition stored by Calc on Sat Jun 15 12:58:03 2019
(put 'calc-define 'calcFunc-deldel '(progn
 (defun calcFunc-deldel (var) (math-check-const var t) (math-normalize
  (list (quote calcFunc-diffop) (list (quote calcFunc-lambda) (quote
  (var T var-T)) (list (quote calcFunc-deriv) (quote (var T var-T))
  var)))))
 (put 'calcFunc-deldel 'calc-user-defn '(calcFunc-diffop (calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var var var-var)))))
 (put 'calcFunc-deldel 'math-compose-forms '((nil (1 lambda (var) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 112 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125))))))) (latex (1 lambda
  (var) (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 102 114 97 99 123 92 112 97 ...))) var
  (list (quote calcFunc-string) (quote (vec 125)))))))))
))

;;; Variable "var-AlgSimpRules" stored by Calc on Sat Jun 15 14:27:00 2019
(setq var-AlgSimpRules '(vec (calcFunc-phase 1) (calcFunc-assign (calcFunc-quote (var del var-del)) (+ (+ (* (var xhat var-xhat) (calcFunc-deldel (var x var-x))) (* (var yhat var-yhat) (calcFunc-deldel (var y var-y)))) (* (var zhat var-zhat) (calcFunc-deldel (var z var-z))))) (calcFunc-assign (calcFunc-por (calcFunc-deedee (var x var-x)) (calcFunc-deldel (var x var-x))) (calcFunc-diffop (calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var x var-x))))) (calcFunc-phase 2) (calcFunc-assign (* (var expr1 var-expr1) (calcFunc-diffop (calcFunc-lambda (var T var-T) (var expr2 var-expr2)))) (calcFunc-diffop (calcFunc-lambda (var T var-T) (* (var expr1 var-expr1) (var expr2 var-expr2))))) (calcFunc-assign (+ (calcFunc-diffop (calcFunc-lambda (var T var-T) (var expr1 var-expr1))) (calcFunc-diffop (calcFunc-lambda (var T var-T) (var expr2 var-expr2)))) (calcFunc-diffop (calcFunc-lambda (var T var-T) (+ (var expr1 var-expr1) (var expr2 var-expr2))))) (calcFunc-phase 3) (calcFunc-assign (calcFunc-diffop (var thunk var-thunk)) (var thunk var-thunk))))

;;; Variable "var-normalize-diffops" stored by Calc on Sat Jun 15 14:27:00 2019
(setq var-normalize-diffops '(vec (calcFunc-assign (calcFunc-deedee (var x var-x)) (calcFunc-diffop (calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var x var-x))))) (calcFunc-assign (calcFunc-deldel (var x var-x)) (calcFunc-diffop (calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var x var-x)))))))

;;; Variable "var-Holidays" stored by Calc on Sat Jun 15 14:27:00 2019
(setq var-Holidays '(vec (var sat var-sat) (var sun var-sun)))

;;; Variable "var-Decls" stored by Calc on Sat Jun 15 14:27:00 2019
(setq var-Decls '(vec))

;;; Variable "var-normalize_diffops" stored by Calc on Sat Jun 15 14:27:00 2019
(setq var-normalize_diffops '(calcFunc-assign (calcFunc-por (calcFunc-deedee (var x var-x)) (calcFunc-deldel (var x var-x))) (calcFunc-diffop (calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var x var-x))))))

;;; Variable "var-normalizediffops" stored by Calc on Sat Jun 15 14:27:01 2019
(setq var-normalizediffops '(calcFunc-assign (calcFunc-por (calcFunc-deedee (var x var-x)) (calcFunc-deldel (var x var-x))) (calcFunc-diffop (calcFunc-lambda (var T var-T) (calcFunc-deriv (var T var-T) (var x var-x))))))

;;; Variable "var-foorewrt" stored by Calc on Sat Jun 15 14:27:01 2019
(setq var-foorewrt '(calcFunc-assign (calcFunc-foo (var x var-x)) (+ (^ (var x var-x) 2) 3)))

;;; Variable "var-foo" stored by Calc on Sat Jun 15 14:27:01 2019
(setq var-foo '(calcFunc-assign (calcFunc-foo (var x var-x)) (+ (^ (var x var-x) 2) 3)))

;;; Variable "var-cartesianToSpherical" stored by Calc on Sat Jun 15 14:27:01 2019
(setq var-cartesianToSpherical '(vec (calcFunc-assign (calcFunc-quote (var x var-x)) (* (var r var-r) (* (calcFunc-sin (var theta var-theta)) (calcFunc-cos (var phi var-phi))))) (calcFunc-assign (calcFunc-quote (var y var-y)) (* (var r var-r) (* (calcFunc-sin (var theta var-theta)) (calcFunc-sin (var phi var-phi))))) (calcFunc-assign (calcFunc-quote (var z var-z)) (* (var r var-r) (calcFunc-cos (var theta var-theta))))))

;;; Variable "var-EvalRules" stored by Calc on Sat Jun 15 14:27:00 2019
(setq var-EvalRules '(vec))

;;; Definition stored by Calc on Sat Jun 15 20:57:00 2019
(put 'calc-define 'calcFunc-grad '(progn
 (defun calcFunc-grad (T) (math-check-const T t) (math-normalize (list
  (quote vec) (list (quote calcFunc-deriv) T (quote (var x var-x)))
  (list (quote calcFunc-deriv) T (quote (var y var-y))) (list (quote
  calcFunc-deriv) T (quote (var z var-z))))))
 (put 'calcFunc-grad 'calc-user-defn '(vec (calcFunc-deriv (var T var-T) (var x var-x)) (calcFunc-deriv (var T var-T) (var y var-y)) (calcFunc-deriv (var T var-T) (var z var-z))))
))

;;; Definition stored by Calc on Sat Jun 15 21:03:47 2019
(put 'calc-define 'calcFunc-curl '(progn
 (defun calcFunc-curl (F) (math-check-const F t) (math-normalize (list
  (quote vec) (list (quote -) (list (quote calcFunc-deriv) (list (quote
  calcFunc-subscr) F 3) (quote (var y var-y))) (list (quote
  calcFunc-deriv) (list (quote calcFunc-subscr) F 2) (quote (var z
  var-z)))) (list (quote -) (list (quote calcFunc-deriv) (list (quote
  calcFunc-subscr) F 1) (quote (var z var-z))) (list (quote
  calcFunc-deriv) (list (quote calcFunc-subscr) F 3) (quote (var x
  var-x)))) (list (quote -) (list (quote calcFunc-deriv) (list (quote
  calcFunc-subscr) F 2) (quote (var x var-x))) (list (quote
  calcFunc-deriv) (list (quote calcFunc-subscr) F 1) (quote (var y
  var-y)))))))
 (put 'calcFunc-curl 'calc-user-defn '(vec (- (calcFunc-deriv (calcFunc-subscr (var F var-F) 3) (var y var-y)) (calcFunc-deriv (calcFunc-subscr (var F var-F) 2) (var z var-z))) (- (calcFunc-deriv (calcFunc-subscr (var F var-F) 1) (var z var-z)) (calcFunc-deriv (calcFunc-subscr (var F var-F) 3) (var x var-x))) (- (calcFunc-deriv (calcFunc-subscr (var F var-F) 2) (var x var-x)) (calcFunc-deriv (calcFunc-subscr (var F var-F) 1) (var y var-y)))))
))

;;; Definition stored by Calc on Sat Jun 15 21:06:06 2019
(put 'calc-define 'calcFunc-div '(progn
 (defun calcFunc-div (F) (math-check-const F t) (math-normalize (list
  (quote +) (list (quote +) (list (quote calcFunc-deriv) (list (quote
  calcFunc-subscr) F 1) (quote (var x var-x))) (list (quote
  calcFunc-deriv) (list (quote calcFunc-subscr) F 2) (quote (var y
  var-y)))) (list (quote calcFunc-deriv) (list (quote calcFunc-subscr) F
  3) (quote (var z var-z))))))
 (put 'calcFunc-div 'calc-user-defn '(+ (+ (calcFunc-deriv (calcFunc-subscr (var F var-F) 1) (var x var-x)) (calcFunc-deriv (calcFunc-subscr (var F var-F) 2) (var y var-y))) (calcFunc-deriv (calcFunc-subscr (var F var-F) 3) (var z var-z))))
))

;;; Mode settings stored by Calc on Wed Jun 19 21:41:44 2019
(setq calc-symbolic-mode t)
(setq calc-algebraic-mode t)
(setq calc-angle-mode 'rad)
(setq calc-complex-mode 'polar)
;;; End of mode settings


;;; Definition stored by Calc on Thu Jun 20 18:59:06 2019
(put 'calc-define 'calcFunc-conj '(progn
 (put 'calcFunc-conj 'math-compose-forms '((nil (1 lambda (z) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 111 118 101 114 108 105 110 101 123)))
  z (list (quote calcFunc-string) (quote (vec 125)))))))))
))

;;; Definition stored by Calc on Thu Jun 20 19:28:31 2019
(put 'calc-define 'calcFunc-abssqr '(progn
 (put 'calcFunc-abssqr 'math-compose-forms '((latex (1 lambda (z) (list
  (quote ^) (list (quote calcFunc-abs) z) 2)))))
))

;;; Variable "var-zzconj" stored by Calc on Thu Jun 20 19:31:50 2019
(setq var-zzconj '(vec (calcFunc-assign (* (var z var-z) (calcFunc-conj (var z var-z))) (calcFunc-abssqr (var z var-z))) (calcFunc-assign (calcFunc-abssqr (calcFunc-conj (var z var-z))) (calcFunc-abssqr (var z var-z)))))

;;; Definition stored by Calc on Thu Jun 20 20:53:10 2019
(put 'calc-define 'calcFunc-arbitrarysign '(progn
 (defun calcFunc-arbitrarysign (n) (math-check-const n t)
  (math-normalize (list (quote calcFunc-arbitrarysign) n)))
 (put 'calcFunc-arbitrarysign 'calc-user-defn '(calcFunc-arbitrarysign (var n var-n)))
 (put 'calcFunc-arbitrarysign 'math-compose-forms '((latex (1 lambda
  (n) (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 112 109)))))) (0 lambda nil (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 112 109)))))))))
))

;;; Variable "var-DistribRules" stored by Calc on Thu Jun 20 21:57:35 2019
(setq var-DistribRules '(vec (calcFunc-iterations 1) (calcFunc-assign (* (var x var-x) (calcFunc-select (+ (var a var-a) (var b var-b)))) (+ (* (var x var-x) (calcFunc-select (var a var-a))) (* (var x var-x) (var b var-b)))) (calcFunc-assign (* (var x var-x) (calcFunc-select (calcFunc-sum (var a var-a) (var b var-b) (var c var-c) (var d var-d)))) (calcFunc-sum (* (var x var-x) (calcFunc-select (var a var-a))) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (/ (var x var-x) (calcFunc-select (+ (var a var-a) (var b var-b)))) (/ 1 (+ (/ (calcFunc-select (var a var-a)) (var x var-x)) (/ (var b var-b) (var x var-x))))) (calcFunc-assign (/ (calcFunc-select (+ (var a var-a) (var b var-b))) (var x var-x)) (+ (/ (calcFunc-select (var a var-a)) (var x var-x)) (/ (var b var-b) (var x var-x)))) (calcFunc-assign (/ (calcFunc-sum (calcFunc-select (var a var-a)) (var b var-b) (var c var-c) (var d var-d)) (var x var-x)) (calcFunc-sum (/ (calcFunc-select (var a var-a)) (var x var-x)) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (^ (var x var-x) (calcFunc-select (+ (var a var-a) (var b var-b)))) (* (^ (var x var-x) (calcFunc-select (var a var-a))) (^ (var x var-x) (var b var-b)))) (calcFunc-assign (^ (var x var-x) (calcFunc-select (calcFunc-sum (var a var-a) (var b var-b) (var c var-c) (var d var-d)))) (calcFunc-prod (^ (var x var-x) (calcFunc-select (var a var-a))) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (^ (var x var-x) (calcFunc-select (* (var a var-a) (var b var-b)))) (^ (^ (var x var-x) (var a var-a)) (calcFunc-select (var b var-b)))) (calcFunc-assign (^ (var x var-x) (calcFunc-select (/ (var a var-a) (var b var-b)))) (^ (^ (var x var-x) (var a var-a)) (calcFunc-select (/ 1 (var b var-b))))) (calcFunc-condition (calcFunc-condition (calcFunc-condition (calcFunc-condition (calcFunc-assign (^ (calcFunc-select (+ (var a var-a) (var b var-b))) (var n var-n)) (calcFunc-select (var x var-x))) (calcFunc-integer (var n var-n))) (calcFunc-geq (var n var-n) 2)) (calcFunc-let (var x var-x) (calcFunc-expandpow (+ (var a var-a) (var b var-b)) (var n var-n)))) (calcFunc-quote (calcFunc-matches (var x var-x) (+ (var y var-y) (var z var-z))))) (calcFunc-assign (^ (calcFunc-select (+ (var a var-a) (var b var-b))) (var x var-x)) (+ (* (var a var-a) (^ (calcFunc-select (+ (var a var-a) (var b var-b))) (- (var x var-x) 1))) (* (var b var-b) (^ (calcFunc-select (+ (var a var-a) (var b var-b))) (- (var x var-x) 1))))) (calcFunc-assign (^ (calcFunc-select (* (var a var-a) (var b var-b))) (var x var-x)) (* (^ (var a var-a) (var x var-x)) (^ (calcFunc-select (var b var-b)) (var x var-x)))) (calcFunc-assign (^ (calcFunc-select (calcFunc-prod (var a var-a) (var b var-b) (var c var-c) (var d var-d))) (var x var-x)) (calcFunc-prod (^ (calcFunc-select (var a var-a)) (var x var-x)) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (^ (calcFunc-select (/ (var a var-a) (var b var-b))) (var x var-x)) (/ (^ (calcFunc-select (var a var-a)) (var x var-x)) (^ (var b var-b) (var x var-x)))) (calcFunc-assign (^ (calcFunc-select (neg (var a var-a))) (var x var-x)) (* (^ -1 (var x var-x)) (^ (calcFunc-select (var a var-a)) (var x var-x)))) (calcFunc-assign (calcFunc-plain (neg (calcFunc-select (+ (var a var-a) (var b var-b))))) (- (calcFunc-select (neg (var a var-a))) (var b var-b))) (calcFunc-assign (calcFunc-plain (neg (calcFunc-select (calcFunc-sum (var a var-a) (var b var-b) (var c var-c) (var d var-d))))) (calcFunc-sum (calcFunc-select (neg (var a var-a))) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (calcFunc-plain (neg (calcFunc-select (* (var a var-a) (var b var-b))))) (* (calcFunc-select (neg (var a var-a))) (var b var-b))) (calcFunc-assign (calcFunc-plain (neg (calcFunc-select (/ (var a var-a) (var b var-b))))) (/ (calcFunc-select (neg (var a var-a))) (var b var-b))) (calcFunc-assign (calcFunc-sqrt (calcFunc-select (* (var a var-a) (var b var-b)))) (* (calcFunc-sqrt (calcFunc-select (var a var-a))) (calcFunc-sqrt (var b var-b)))) (calcFunc-assign (calcFunc-sqrt (calcFunc-select (calcFunc-prod (var a var-a) (var b var-b) (var c var-c) (var d var-d)))) (calcFunc-prod (calcFunc-sqrt (calcFunc-select (var a var-a))) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (calcFunc-sqrt (calcFunc-select (/ (var a var-a) (var b var-b)))) (/ (calcFunc-sqrt (calcFunc-select (var a var-a))) (calcFunc-sqrt (var b var-b)))) (calcFunc-assign (calcFunc-sqrt (calcFunc-select (neg (var a var-a)))) (* (calcFunc-sqrt -1) (calcFunc-sqrt (calcFunc-select (var a var-a))))) (calcFunc-condition (calcFunc-assign (calcFunc-exp (calcFunc-select (+ (var a var-a) (var b var-b)))) (/ (calcFunc-exp (calcFunc-select (var a var-a))) (calcFunc-exp (neg (var b var-b))))) (calcFunc-negative (var b var-b))) (calcFunc-assign (calcFunc-exp (calcFunc-select (+ (var a var-a) (var b var-b)))) (* (calcFunc-exp (calcFunc-select (var a var-a))) (calcFunc-exp (var b var-b)))) (calcFunc-assign (calcFunc-exp (calcFunc-select (calcFunc-sum (var a var-a) (var b var-b) (var c var-c) (var d var-d)))) (calcFunc-prod (calcFunc-exp (calcFunc-select (var a var-a))) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-condition (calcFunc-assign (calcFunc-exp (calcFunc-select (* (var a var-a) (var b var-b)))) (^ (calcFunc-exp (calcFunc-select (var a var-a))) (var b var-b))) (calcFunc-constant (var b var-b))) (calcFunc-assign (calcFunc-exp (calcFunc-select (* (var a var-a) (var b var-b)))) (^ (calcFunc-exp (calcFunc-select (var a var-a))) (var b var-b))) (calcFunc-assign (calcFunc-exp (calcFunc-select (/ (var a var-a) (var b var-b)))) (^ (calcFunc-exp (calcFunc-select (var a var-a))) (/ 1 (var b var-b)))) (calcFunc-assign (calcFunc-ln (calcFunc-select (* (var a var-a) (var b var-b)))) (+ (calcFunc-ln (calcFunc-select (var a var-a))) (calcFunc-ln (var b var-b)))) (calcFunc-assign (calcFunc-ln (calcFunc-select (calcFunc-prod (var a var-a) (var b var-b) (var c var-c) (var d var-d)))) (calcFunc-sum (calcFunc-ln (calcFunc-select (var a var-a))) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (calcFunc-ln (calcFunc-select (/ (var a var-a) (var b var-b)))) (- (calcFunc-ln (calcFunc-select (var a var-a))) (calcFunc-ln (var b var-b)))) (calcFunc-assign (calcFunc-ln (calcFunc-select (^ (var a var-a) (var b var-b)))) (* (calcFunc-ln (calcFunc-select (var a var-a))) (var b var-b))) (calcFunc-assign (calcFunc-log10 (calcFunc-select (* (var a var-a) (var b var-b)))) (+ (calcFunc-log10 (calcFunc-select (var a var-a))) (calcFunc-log10 (var b var-b)))) (calcFunc-assign (calcFunc-log10 (calcFunc-select (calcFunc-prod (var a var-a) (var b var-b) (var c var-c) (var d var-d)))) (calcFunc-sum (calcFunc-log10 (calcFunc-select (var a var-a))) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (calcFunc-log10 (calcFunc-select (/ (var a var-a) (var b var-b)))) (- (calcFunc-log10 (calcFunc-select (var a var-a))) (calcFunc-log10 (var b var-b)))) (calcFunc-assign (calcFunc-log10 (calcFunc-select (^ (var a var-a) (var b var-b)))) (* (calcFunc-log10 (calcFunc-select (var a var-a))) (var b var-b))) (calcFunc-assign (calcFunc-log (calcFunc-select (* (var a var-a) (var b var-b))) (var x var-x)) (+ (calcFunc-log (calcFunc-select (var a var-a)) (var x var-x)) (calcFunc-log (var b var-b) (var x var-x)))) (calcFunc-assign (calcFunc-log (calcFunc-select (calcFunc-prod (var a var-a) (var b var-b) (var c var-c) (var d var-d))) (var x var-x)) (calcFunc-sum (calcFunc-log (calcFunc-select (var a var-a)) (var x var-x)) (var b var-b) (var c var-c) (var d var-d))) (calcFunc-assign (calcFunc-log (calcFunc-select (/ (var a var-a) (var b var-b))) (var x var-x)) (- (calcFunc-log (calcFunc-select (var a var-a)) (var x var-x)) (calcFunc-log (var b var-b) (var x var-x)))) (calcFunc-assign (calcFunc-log (calcFunc-select (^ (var a var-a) (var b var-b))) (var x var-x)) (* (calcFunc-log (calcFunc-select (var a var-a)) (var x var-x)) (var b var-b))) (calcFunc-assign (calcFunc-log (var a var-a) (calcFunc-select (var b var-b))) (/ (calcFunc-ln (var a var-a)) (calcFunc-select (calcFunc-ln (var b var-b))))) (calcFunc-assign (calcFunc-sin (calcFunc-select (+ (var a var-a) (var b var-b)))) (+ (* (calcFunc-sin (calcFunc-select (var a var-a))) (calcFunc-cos (var b var-b))) (* (calcFunc-cos (var a var-a)) (calcFunc-sin (var b var-b))))) (calcFunc-assign (calcFunc-sin (calcFunc-select (* 2 (var a var-a)))) (* 2 (* (calcFunc-sin (calcFunc-select (var a var-a))) (calcFunc-cos (var a var-a))))) (calcFunc-condition (calcFunc-condition (calcFunc-assign (calcFunc-sin (calcFunc-select (* (var n var-n) (var a var-a)))) (- (* 2 (* (calcFunc-sin (* (- (var n var-n) 1) (calcFunc-select (var a var-a)))) (calcFunc-cos (var a var-a)))) (calcFunc-sin (* (- (var n var-n) 2) (var a var-a))))) (calcFunc-integer (var n var-n))) (calcFunc-gt (var n var-n) 2)) (calcFunc-assign (calcFunc-cos (calcFunc-select (+ (var a var-a) (var b var-b)))) (- (* (calcFunc-cos (calcFunc-select (var a var-a))) (calcFunc-cos (var b var-b))) (* (calcFunc-sin (var a var-a)) (calcFunc-sin (var b var-b))))) (calcFunc-assign (calcFunc-cos (calcFunc-select (* 2 (var a var-a)))) (- (* 2 (^ (calcFunc-cos (calcFunc-select (var a var-a))) 2)) 1)) (calcFunc-condition (calcFunc-condition (calcFunc-assign (calcFunc-cos (calcFunc-select (* (var n var-n) (var a var-a)))) (- (* 2 (* (calcFunc-cos (* (- (var n var-n) 1) (calcFunc-select (var a var-a)))) (calcFunc-cos (var a var-a)))) (calcFunc-cos (* (- (var n var-n) 2) (var a var-a))))) (calcFunc-integer (var n var-n))) (calcFunc-gt (var n var-n) 2)) (calcFunc-assign (calcFunc-tan (calcFunc-select (+ (var a var-a) (var b var-b)))) (/ (+ (calcFunc-tan (calcFunc-select (var a var-a))) (calcFunc-tan (var b var-b))) (- 1 (* (calcFunc-tan (var a var-a)) (calcFunc-tan (var b var-b)))))) (calcFunc-assign (calcFunc-tan (calcFunc-select (* 2 (var a var-a)))) (/ (* 2 (calcFunc-tan (calcFunc-select (var a var-a)))) (- 1 (^ (calcFunc-tan (var a var-a)) 2)))) (calcFunc-condition (calcFunc-condition (calcFunc-assign (calcFunc-tan (calcFunc-select (* (var n var-n) (var a var-a)))) (/ (+ (calcFunc-tan (* (- (var n var-n) 1) (calcFunc-select (var a var-a)))) (calcFunc-tan (var a var-a))) (- 1 (* (calcFunc-tan (* (- (var n var-n) 1) (var a var-a))) (calcFunc-tan (var a var-a)))))) (calcFunc-integer (var n var-n))) (calcFunc-gt (var n var-n) 2)) (calcFunc-assign (calcFunc-cot (calcFunc-select (+ (var a var-a) (var b var-b)))) (/ (- (* (calcFunc-cot (calcFunc-select (var a var-a))) (calcFunc-cot (var b var-b))) 1) (+ (calcFunc-cot (var a var-a)) (calcFunc-cot (var b var-b))))) (calcFunc-assign (calcFunc-sinh (calcFunc-select (+ (var a var-a) (var b var-b)))) (+ (* (calcFunc-sinh (calcFunc-select (var a var-a))) (calcFunc-cosh (var b var-b))) (* (calcFunc-cosh (var a var-a)) (calcFunc-sinh (var b var-b))))) (calcFunc-assign (calcFunc-cosh (calcFunc-select (+ (var a var-a) (var b var-b)))) (+ (* (calcFunc-cosh (calcFunc-select (var a var-a))) (calcFunc-cosh (var b var-b))) (* (calcFunc-sinh (var a var-a)) (calcFunc-sinh (var b var-b))))) (calcFunc-assign (calcFunc-tanh (calcFunc-select (+ (var a var-a) (var b var-b)))) (/ (+ (calcFunc-tanh (calcFunc-select (var a var-a))) (calcFunc-tanh (var b var-b))) (+ 1 (* (calcFunc-tanh (var a var-a)) (calcFunc-tanh (var b var-b)))))) (calcFunc-assign (calcFunc-coth (calcFunc-select (+ (var a var-a) (var b var-b)))) (/ (+ (* (calcFunc-coth (calcFunc-select (var a var-a))) (calcFunc-coth (var b var-b))) 1) (+ (calcFunc-coth (var a var-a)) (calcFunc-coth (var b var-b))))) (calcFunc-assign (calcFunc-land (var x var-x) (calcFunc-select (calcFunc-lor (var a var-a) (var b var-b)))) (calcFunc-lor (calcFunc-land (var x var-x) (calcFunc-select (var a var-a))) (calcFunc-land (var x var-x) (var b var-b)))) (calcFunc-assign (calcFunc-land (calcFunc-select (calcFunc-lor (var a var-a) (var b var-b))) (var x var-x)) (calcFunc-lor (calcFunc-land (calcFunc-select (var a var-a)) (var x var-x)) (calcFunc-land (var b var-b) (var x var-x)))) (calcFunc-assign (calcFunc-lnot (calcFunc-select (calcFunc-land (var a var-a) (var b var-b)))) (calcFunc-lor (calcFunc-lnot (var a var-a)) (calcFunc-lnot (var b var-b)))) (calcFunc-assign (calcFunc-lnot (calcFunc-select (calcFunc-lor (var a var-a) (var b var-b)))) (calcFunc-land (calcFunc-lnot (var a var-a)) (calcFunc-lnot (var b var-b)))) (calcFunc-condition (calcFunc-assign (* (calcFunc-select (var a var-a)) (var z var-z)) (+ (calcFunc-select (* (var z var-z) (calcFunc-re (var a var-a)))) (* (* (var z var-z) (calcFunc-im (var a var-a))) (var i var-i)))) (calcFunc-neq (calcFunc-im (var a var-a)) 0))))

;;; Definition stored by Calc on Sat Jun 22 20:43:39 2019
(put 'calc-define 'calcFunc-exp '(progn
 (put 'calcFunc-exp 'math-compose-forms '((latex (1 lambda (x) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 101 94 123))) x (list (quote
  calcFunc-string) (quote (vec 125)))))))))
))

;;; Definition stored by Calc on Sat Jun 22 22:21:41 2019
(put 'calc-define 'calcFunc-paren '(progn
 (defun calcFunc-paren (x) (math-check-const x t) (math-normalize x))
 (put 'calcFunc-paren 'calc-user-defn '(var x var-x))
 (put 'calcFunc-paren 'math-compose-forms '((latex (1 lambda (x) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 108 101 102 116 40 32))) x (list
  (quote calcFunc-string) (quote (vec 32 92 114 105 103 104 116
  41)))))))))
))

;;; Definition stored by Calc on Sat Jun 22 22:31:41 2019
(put 'calc-define 'calcFunc-abs '(progn
))

;;; Definition stored by Calc on Sat Jun 22 22:32:36 2019
(put 'calc-define 'calcFunc-abs '(progn
 (put 'calcFunc-abs 'math-compose-forms '((nil (1 lambda (x) (list
  (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 124))) x (list (quote calcFunc-string)
  (quote (vec 124)))))))))
))

;;; Variable "var-liftselected" stored by Calc on Tue Jul  2 18:39:35 2019
(setq var-liftselected '(vec (calcFunc-assign (calcFunc-select (calcFunc-pand (var x var-x) (calcFunc-pnot (calcFunc-selected (var y var-y))))) (calcFunc-select (calcFunc-selected (var x var-x))))))



;;; Definition stored by Calc on Tue Jul  2 07:11:34 2019
(put 'calc-define 'calcFunc-selected '(progn
 (defun calcFunc-selected (x) (math-check-const x t) (math-normalize
  x))
 (put 'calcFunc-selected 'calc-user-defn '(var x var-x))
))

;;; Definition stored by Calc on Tue Jul  2 07:12:20 2019
(put 'calc-define 'calcFunc-selected '(progn
 (defun calcFunc-selected (x) (math-check-const x t) (math-normalize
  x))
 (put 'calcFunc-selected 'calc-user-defn '(var x var-x))
 (put 'calcFunc-selected 'math-compose-forms '((latex (1 lambda (x)
  (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 109 97 116 104 99 111 108 111 114 98
  111 120 123 114 101 100 125 123))) x (list (quote calcFunc-string)
  (quote (vec 125)))))))))
))

;;; Definition stored by Calc on Tue Jul  2 07:37:57 2019
(put 'calc-define 'calcFunc-selected '(progn
 (defun calcFunc-selected (x) (math-check-const x t) (math-normalize
  x))
 (put 'calcFunc-selected 'calc-user-defn '(var x var-x))
 (put 'calcFunc-selected 'math-compose-forms '((latex (1 lambda (x)
  (list (quote calcFunc-choriz) (list (quote vec) (list (quote
  calcFunc-string) (quote (vec 92 109 97 116 104 99 111 108 111 114 123
  114 101 100 125 123))) x (list (quote calcFunc-string) (quote (vec
  125)))))))))
))

;;; Variable "var-hideselected" stored by Calc on Tue Jul  2 18:38:12 2019
(setq var-hideselected '(vec (calcFunc-assign (calcFunc-selected (var x var-x)) (var x var-x))))
