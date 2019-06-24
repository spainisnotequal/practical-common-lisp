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
