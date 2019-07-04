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
(vector-pop *v*)     ; => ERROR - "There is nothing left to pop."

;;; --------------------
;;; Length of a sequence
;;; --------------------

(defparameter *v* (vector -3 9 5))

(length *v*) ; => 3

;;; ------------------------------------
;;; Access to the elements of a sequence
;;; ------------------------------------

(defparameter *v* (vector -3 9 5))

(elt *v* 0) ; => -3
(elt *v* 1) ; => 9
(elt *v* 2) ; => 5
(elt *v* 3) ; => ERROR - "The index 3 is too large."

;; ELT is a SETFable place, so you can set the value of a particular element
;; like this:
(setf (elt *v* 1) -11) ; => -11
*v*                    ; => #(-3 -11 5)

;;; -----------------------------------------------------
;;; Finding and filtering specific elements of a sequence
;;; -----------------------------------------------------

(count 1 '(1 2 3 1 2 3)) ; => 2

(remove 1 #(1 2 3 1 2 3)) ; => #(2 3 2 3)
(remove 1 '(1 2 3 1 2 3)) ; => (2 3 2 3)
(remove #\a "foobarbaz")  ; => "foobrbz"

(substitute 9 1 #(1 2 3 1 2 3))  ; => #(9 2 3 9 2 3)
(substitute 9 1 '(1 2 3 1 2 3))  ; => (9 2 3 9 2 3)
(substitute #\A #\a "foobarbaz") ; => "foobArbAz"

(find 1 #(1 2 3 1 2 3)) ; => 1
(find 1 '(1 2 3 1 2 3)) ; => 1
(find #\a "foobarbaz")  ; => #\a

(position 1 '(1 2 3 1 2 3)) ; => 0
(position #\a "foobarbaz")  ; => 4

;; We can use keyword arguments to modify the behaviour of these functions

;; :test keyword
(remove 2 '(1 2 3 1 2 3)
        :test #'>=)       ; => (3 3)
(remove 2 '(1 2 3 1 2 3)
        :test #'(lambda (x y) (>= x y))) ; => (3 3)

(remove 2 '(1 2 3 1 2 3)
        :test (complement #'>=)) ; => (1 2 1 2)

;; :key keyword
(remove 2 '((1 "one") (2 "two") (3 "three"))
        :key #'first)                        ; => ((1 "one") (3 "three"))
(remove 2 '((1 "one") (2 "two") (3 "three"))
        :test #'>=
        :key #'first)                        ; => (3 "three")

;; :from-end keyword
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)             ; => (A 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; => (A 30)
