;;;; -------------------- ;;;;
;;;; Create New Variables ;;;;
;;;; -------------------- ;;;;

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

(defun foo (x)
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
