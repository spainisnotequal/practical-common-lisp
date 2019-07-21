;;;; --------------------- ;;;;
;;;; OTHER DATA STRUCTURES ;;;;
;;;; --------------------- ;;;;

;;; -----
;;; Lists
;;; -----

;; Difference between a "dotted list" and a "proper list"
(cons 1 (cons 2 (cons 3 4)))            ; => (1 2 3 . 4)
(cons 1 (cons 2 (cons 3 (cons 4 nil)))) ; =>(1 2 3 4)

;;; -----
;;; Trees
;;; -----

(defparameter *l1* (list 1 2))
(defparameter *l2* (list 3 4))
(defparameter *l3* (list 5 6))

(defparameter *t1* (list *l1* *l2* *l3*))

(defparameter *t2* (copy-tree *t1*))

(tree-equal *t1* *t2*) ; => T

(setf (first *l3*) 1)

*t1* ; => ((1 2) (3 4) (1 6))
*t2* ; => ((1 2) (3 4) (5 6))

(tree-equal *t1* *t2*) ; => NIL

(subst -1 1 *t1*) ; => ((-1 2) (3 4) (-1 6))
*t1*              ; => ((1 2) (3 4) (1 6))

(subst-if nil #'oddp *t1*) ; => ; Evaluation aborted on #<TYPE-ERROR expected-type: INTEGER datum: ((1 2) (3 4) (1 6))>.

;; the previous evaluation produces an error because we are passing to the
;; whole tree ((1 2) (3 4) (1 6)) to the predicate ODDP, which expects a number,
;; not a list.
;; The reason is that the cons cells of the tree are processed by SUSBT-IF recursively, first their CAR and later their CONS. So the elements that are
;; passed to ODDP are, in order:
;;     ((1 2) (3 4) (5 6)) 
;;     (1 2) 
;;     1 
;;     (2) 
;;     2 
;;     ((3 4) (5 6)) 
;;     (3 4) 
;;     3 
;;     (4) 
;;     4 
;;     ((5 6)) 
;;     (5 6) 
;;     5 
;;     (6) 
;;     6
;; To process that tree correctly, we should prevent passing a list to ODDP (this
;; function only accepts INTEGERS). We could do that in this way:
(subst-if 'yeah
          #'(lambda (element)
              (when (integerp element)
                (oddp element)))
          *t1*)                        ; => ((YEAH 2) (YEAH 4) (YEAH 6))

;; In the next example, firstly we evaluate (LISTP *t1*), which happens to be T,
;; so we substitute the whole tree by 'YEAH, and, because 'YEAH it's not a cons
;; cell, SUBST-IF ends.
(subst-if 'yeah
          #'(lambda (element)
              (listp element))
          *t1*)                        ; => YEAH
;;; ----
;;; Sets
;;; ----


;;; -------------
;;; Lookup tables
;;; -------------


;;; ------------------------
;;; Destructuring-bind macro
;;; ------------------------
    
