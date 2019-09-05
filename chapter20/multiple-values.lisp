;;; -----------------------------------
;; Use VALUES to return multiple values
;;; -----------------------------------
(values 1 'uno) ; => 1, UNO

;;; -----------------------------------------------------------------
;;; Use MULTIPLE-VALUE-BIND to bind some variables to multiple values
;;; -----------------------------------------------------------------
(floor 5 2) ; => 2, 1

(multiple-value-bind (quotient remainder) (floor 5 2)
  (format nil "quotient = ~a; remainder = ~a"quotient remainder)) ; => "quotient = 2; remainder = 1"

;;; ---------------------------------------------------------
;;; Use MULTIPLE-VALUE-LIST to bind multiple values to a list
;;; ---------------------------------------------------------

(multiple-value-list (values 1 'uno)) ; => (1 UNO)

;; NOTE:  if you find yourself using MULTIPLE-VALUE-LIST a lot, it may be a sign
;; that some function should be retugrningg a list to start with rather than
;; multiple values

;;; ------------------------------------------------------------------
;;; Use SETF and VALUES to assign multiple values to existin variables
;;; ------------------------------------------------------------------

(defparameter *x* nil)
(defparameter *y* nil)

(setf (values *x* *y*) (floor 5 2)) ; => 2, 1

*x* ; => 2
*y* ; => 1
