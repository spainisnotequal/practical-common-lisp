;;;; ------------------- ;;;;
;;;; NUMERIC COMPARISONS ;;;;
;;;; ------------------- ;;;;

;;; --------
;;; Equality
;;; --------

(= 10 10.0) ; => T
(= 10 20/2) ; => T
(= 10 10.0 20/2 #c (10 0)) ; => T

(eq 10 10.0) ; => NIL
(eq 10 20/2) ; => T

;;; ------------
;;; Non equality
;;; ------------

(/= 1 1) ; => NIL
(/= 1 2) ; => T
(/= 1 2 3) ; => T

;;; --------------------------
;;; Maximun and minimum values
;;; --------------------------

(max 1 2 3)    ; => 3
(min -1 -2 -3) ; => -3

;;; -----------------------------------------
;;; Equal to, less than, or greater than zero
;;; -----------------------------------------

(zerop -1)  ; => NIL
(zerop 0)   ; => T
(zerop 1)   ; => NIL

(minusp -1) ; => T
(minusp 0)  ; => NIL
(minusp 1)  ; => NIL

(plusp -1)  ; => NIL
(plusp 0)   ; => NIL
(plusp 1)   ; => T

;;; ----------------------------------
;;; Even or odd (only integer numbers)
;;; ----------------------------------

(evenp 0) ; => T
(evenp 1) ; => NIL
(evenp 2) ; => T

(oddp 0) ; => NIL
(oddp 1) ; => T
(oddp 2) ; => NIL

(evenp 1.0) ; => ERROR: Argument X is not a INTEGER: 1.0
(oddp -1.0) ; => ERROR: Argument X is not a INTEGER: -1.0
