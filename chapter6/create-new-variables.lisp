;;;; ---------------------------- ;;;;
;;;; Create new lexical variables ;;;;
;;;; ---------------------------- ;;;;

;; ----------------------
;; As function parameters
;; ----------------------

(defun foo (x y z)
  (+ x y z))

;; --------------------------------
;; Using the special operator "LET"
;; --------------------------------

(let ((x 10)
      (y 20)
      z)
  (list x y z)) ; => (10 20 NIL)

3(defun foo (x)
  (format t "Parameter: ~a~%" x)      ; |<------ x is argument 
  (let ((x 2))                        ; |
    (format t "Outer LET: ~a~%" x)    ; | |<---- x is 2
    (let ((x 3))                      ; | |
      (format t "Inner LET: ~a~%" x)) ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))   ; | |
  (format t "Parameter: ~a~%" x))

(foo 1)

;; ---------------------------------
;; Using the special operator "LET*"
;; ---------------------------------

(let* ((x 10)
       (y (+ x 10)))
  (list x y))

;; equivalent to the following expressions if we only use "LET" but not "LET*"
(let ((x 10))
  (let ((y (+ x 10)))
    (list x y)))

;; --------------------------------------
;; Using other Lisp forms, like "DOTIMES"
;; --------------------------------------

(defun foo (x)
  (format t "Parameter: ~a~%" x)
  (dotimes (x 10)
    (format t "Loop: ~a~%" x))
  (format t "Parameter: ~a~%" x))

(foo 33)


;;;; ---------------------------- ;;;;
;;;; Create new dynamic variables ;;;;
;;;; ---------------------------- ;;;;

(defparameter x 0)
(defvar y 0)
(format t "x = ~a~%" x) ; => x = 0
(format t "y = ~a~%" y) ; => y = 0

(defparameter x 1)
(defvar y 1)
(format t "x = ~a~%" x) ; => x = 1
(format t "y = ~a~%" y) ; => y = 0
