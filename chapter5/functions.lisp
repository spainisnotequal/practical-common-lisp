;;; ------------------------
;;; Function Parameter Lists
;;; ------------------------

;; -------------------
;; Required parameters
;; -------------------

(defun create-list (a b c)
  (list a b c))

(create-list 1 2 3)   ; => (1 2 3)
(create-list 1 2)     ; => ERROR: invalid number of arguments: 2
(create-list 1)       ; => ERROR: invalid number of arguments: 1
(create-list)         ; => ERROR: invalid number of arguments: 0
(create-list 1 2 3 4) ; => ERROR: invalid number of arguments: 4

;; -------------------
;; Optional parameters
;; -------------------

(defun create-list (a b &optional (c 0 c-supplied-p))
  (list a b c c-supplied-p))

(create-list 1 2 3 ); => (1 2 3 T)
(create-list 1 2)   ; => (1 2 0 NIL)
(create-list 1)     ; => ERROR: invalid number of arguments: 1
(create-list)       ; => ERROR: invalid number of arguments: 0

(defun create-list (a b &optional (c (/ a b)))
  (list a b c))

(create-list 1 2)   ; => (1 2 1/2)
(create-list 1 2 3) ; => (1 2 3)

;; ---------------
;; Rest parameters
;; ---------------

(defun create-list (a b &rest rest)
  (list a b rest))

(create-list 1 2 3) ; => (1 2 (3))
(create-list 1 2 3 4) ; => (1 2 (3 4))

;; ------------------
;; Keyword parameters
;; ------------------

(defun create-list (&key a b c)
  (list a b c))

(create-list)                 ; => (NIL NIL NIL)
(create-list :a 1)            ; => (1 NIL NIL)
(create-list :b 1)            ; => (NIL 1 NIL)
(create-list :c 1)            ; => (NIL NIL 1)
(create-list :a 1 :c 1)       ; => (1 NIL 1)
(create-list :c 1 :b 0)       ; => (NIL 0 1)
(create-list :c 1 :a -6 :b 0) ; => (-6 0 1)

(defun create-list (&key (a 0) (b 0 b-supplied-p) (c 0))
  (list a b c b-supplied-p))

(create-list)                 ; => (0 0 0 NIL)
(create-list :a 1)            ; => (1 0 0 NIL)
(create-list :b 1)            ; => (0 1 0 T)
(create-list :c 1)            ; => (0 0 1 NIL)
(create-list :a 1 :c 1)       ; => (1 0 1 NIL)
(create-list :c 1 :b 0)       ; => (0 0 1 T)
(create-list :c 1 :a -6 :b 0) ; => (-6 0 1 T)

;; --------------------------------
;; Mixing different parameter types
;; --------------------------------

(defun foo (x &optional y &key z)
  (list x y z))

(foo 1 2 :z 3) ; => (1 2 3)
(foo 1)        ; => (1 NIL NIL)
(foo 1 :z 3)   ; => ERROR: odd number of &KEY arguments

;;; ----------------------------------------
;;; Function as Data. Higher-Order Functions
;;; ----------------------------------------

(defun by-2 (x)
  (* 2 x))

;; #' is syntactic sugar for the function named "function",
;; which returns the function object
(function by-2) ; => #<FUNCTION BY-2>
#'by-2          ; => #<FUNCTION BY-2>

;; Using functions as arguments of other functions
(defun square (x)
  (* x x))

(defun cube (x)
  (* x x x))

(defun generate-sequence (fn min max step)
  (loop for i from min to max by step do
       (format t "~d~%" (funcall fn i))))

(generate-sequence #'square 1 10 1)
(generate-sequence #'cube 1 10 1)


;;; -------------------
;;; Anonymous functions
;;; -------------------

(generate-sequence #'(lambda (x) (* x x)) 1 10 1)
(generate-sequence #'(lambda (x) (* x x x)) 1 10 1)
