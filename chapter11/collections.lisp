;;;; ----------- ;;;;
;;;; COLLECTIONS ;;;;
;;;; ----------- ;;;;

;;; -------------------------
;;; Create vectors and arrays
;;; -------------------------

(vector)     ; -> #()
(vector 1)   ; => #(1)
(vector 1 2) ; => #(1 2)

(make-array 4) ; => #(0 0 0 0)
(make-array 4 :initial-element nil) ; => #(NIL NIL NIL NIL)
(make-array 4 :fill-pointer 0) ; => #()
(make-array 4 :fill-pointer 2) ; => #(0 0) 

;;; ----------------------
;;; Add or delete elements
;;; ----------------------

(defparameter *v* (make-array 3 :fill-pointer 2)) ; => *V*
*v*                  ; => #(0 0)
(vector-push 33 *v*) ; =>2
*v*                  ; => #(0 0 33)
(vector-push 33 *v*) ; => NIL
*v*                  ; => #(0 0 33)
(vector-pop *v*)     ; => 33
*v*                  ; => #(0 0)
(vector-pop *v*)     ; => 0
*v*                  ; => #(0)
(vector-pop *v*)     ; => 0
*v*                  ; => #()
(vector-pop *v*)     ; => Evaluation aborted on #<SIMPLE-ERROR "There is nothing
                     ; left to pop." {51AF1369}>.
