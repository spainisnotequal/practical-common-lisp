;;;; --------------------------- ;;;;
;;;; LIST-MANIPULATION FUNCTIONS ;;;;
;;;; --------------------------- ;;;;

;;; --------------------------------
;;; Accessing the elements of a list
;;; --------------------------------

(car (list 1 2 3)) ; => 1
(cdr (list 1 2 3)) ; => 1

(first (list 1 2 3)) ; => 1
(rest (list 1 2 3))  ; => (2 3)

(second (list 1 2 3))              ; => 2
(third (list 1 2 3))               ; => 3
(tenth (list 0 1 2 3 4 5 6 7 8 9)) ; => 9
(tenth (list 1 2 3))               ; => NIL

(nth 2 (list 1 2 3)) ; => 3
(nth 3 (list 1 2 3)) ; => NIL

(last (list 1 2 3))    ; => (3)
(butlast (list 1 2 3)) ; => (1 2)

;;; ----------
;;; Predicates
;;; ----------

(null 33)           ; => NIL
(null (cons 1 2))   ; => NIL
(null (list 1 2 3)) ; => NIL
(null '())          ; => T

(atom 33)           ; => T
(atom (cons 1 2))   ; => NIL
(atom (list 1 2 3)) ; => NIL
(atom '())          ; => T

(consp 33)           ; => NIL
(consp (cons 1 2))   ; => T
(consp (list 1 2 3)) ; => T
(consp '())          ; => NIL

(listp 33)           ; => NIL
(listp (cons 1 2))   ; => T
(listp (list 1 2 3)) ; => T
(listp '())          ; => T
