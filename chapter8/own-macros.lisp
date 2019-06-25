;;;; --------------- ;;;;
;;;; DO-PRIMES macro ;;;;
;;;; --------------- ;;;;

;;; -------------------
;;; Auxiliary functions
;;; -------------------

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;;; -------------------------------------------------------------
;;; STEP 1: Macro call example and the Lisp form it should expand
;;; -------------------------------------------------------------

;; this macro call example:
(do-primes (prime-number 0 19)
  (print prime-number))
;; should expand to:
(do ((prime-number (next-prime 0) (next-prime (1+ prime-number))))
    ((> prime-number 19))
  (print prime-number))

;;; -----------------------
;;; STEP 2: Write the macro
;;; -----------------------

;; First version:
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

(do-primes (p 0 19) (print p)) ; prints 2 3 5 7 11 13 17 19

;; Second version:
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

(do-primes (p 0 19) (print p)) ; prints 2 3 5 7 11 13 17 19

;; Third version (not an improvement, just to show how handy is the backquote):
(defmacro do-primes ((var start end) &body body)
  (append '(do)
          (list  (list (list var
                             (list 'next-prime start)
                             (list 'next-prime (list '1+ var)))))
          (list (list (list '> var end)))
          body))

(do-primes (p 0 19) (print p)) ; prints 2 3 5 7 11 13 17 19

;;; ----------------------------------------------------------------
;;; STEP 3: Check that the macro doesn't leak implementation details
;;; ----------------------------------------------------------------

;; DOTIMES doesn't evaluate the end subform too many times, but DO-PRIMES do, so
;; we need to fix that leak

(dotimes (x (random 10)) (print x)) ; expands to:
(DO ((X 0 (1+ X))
     (#:G524 (THE INTEGER (RANDOM 10))))
    ((>= X #:G524) NIL)
  (DECLARE (TYPE UNSIGNED-BYTE X))
  (PRINT X))

(do-primes (x 0 (random 100)) (print x)) ; expands to:
(DO ((X (NEXT-PRIME 0) (NEXT-PRIME (1+ X))))
    ((> X (RANDOM 100))) ; evaluates (random 100) each loop
  (PRINT X))

;; We can easily fix that issue by defining a new variable in the initialization
;; form (without a step form, so it doesn't change between loops):
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

(do-primes (x 0 (random 100)) (print x)) ; expands to:
(DO ((ENDING-VALUE (RANDOM 100))
     (X (NEXT-PRIME 0) (NEXT-PRIME (1+ X))))
    ((> X ENDING-VALUE))
  (PRINT X))

;; But now we have introduced two leaks. The first leak was introduced because
;; we evaluate the end expression before the start expression. If those
;; expressions are just literal values (numbers in this examples) there is no
;; problem; but if they are forms that introduce side effects, we can have
;; unexpected results.
;; To fix that issue, just exchange the order of the two variable definitions:
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))

(do-primes (x 0 (random 100)) (print x)) ; expands to:
(DO ((X (NEXT-PRIME 0) (NEXT-PRIME (1+ X)))
     (ENDING-VALUE (RANDOM 100)))
    ((> X ENDING-VALUE))
  (PRINT X))

;; The other leak we introduced is due to the name of the new variable,
;; "ending-value". The name of that variable (who is a just an internal detail
;; of the macro) may cause some problems when we interact with the macro. For
;; example, consider the following innocent call to DO-PRIMES:

(do-primes (ending-value 0 10) (print ending-value)) ; expands to:
(DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE)))
     (ENDING-VALUE 10))
    ((> ENDING-VALUE ENDING-VALUE))
  (PRINT ENDING-VALUE))

;; We can see how the end-test-form:
;; (> ENDING-VALUE ENDING-VALUE)
;; is going to cause problems.
;; Some Lisp implementations, like SBCL, will catch this problem and will not
;; evaluate this expression because the ENDING-VALUE is used twice as a variable
;; name in the same loop (the exact error says: "The variable ENDING-VALUE
;; occurs more than once in the LET.").
;; To solve this problem, we need to use the function GENSYM, which returns an
;; unique symbol each time it's called, and assign that unique symbol to the
;; of the END variable:
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(do-primes (ending-value 0 10) (print ending-value)) ; expands to:
(DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE)))
     (#:G538 10))
    ((> ENDING-VALUE #:G538))
  (PRINT ENDING-VALUE))

;; Now, the variable used to hold the ending value is the gensymed symbol #:G538
