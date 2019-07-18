;;;; --------------------------- ;;;;
;;;; LIST-MANIPULATION FUNCTIONS ;;;;
;;;; --------------------------- ;;;;

;;; ---
;;; MAP
;;; ---

(map 'list
     #'(lambda (x) (+ x (* 10 x)))
     (list 1 2 3))                 ; => (11 22 33)
(map 'vector
     #'+ (list 1 2 3)
     (list 10 20 30))              ; => #(11 22 33)

;;; ------
;;; MAPCAR
;;; ------

(mapcar #'(lambda (x) (+ x (* 10 x)))
        (list 1 2 3))                ; => (11 22 33)
(mapcar #'+ (list 1 2 3)
        (list 10 20 30))             ; => (11 22 33)

(mapcar #'car
        '((1 a) (2 b) (3 c))) ; => (1 2 3) 
(mapcar #'abs
        '(3 -4 2 -5 -6))      ; => (3 4 2 5 6)
(mapcar #'cons
        '(a b c)
        '(1 2 3))             ; => ((A . 1) (B . 2) (C . 3))


;;; -------
;;; MAPLIST
;;; -------

(maplist #'append
         (list 1 2 3 4)
         (list 1 2))   ; => ((1 2 3 4 1 2) (2 3 4 2))

(maplist #'(lambda (x)
             (cons 'foo x))
         (list 'a 'b 'c 'd)) ; => ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D))
(maplist #'(lambda (x)
             (if (member (car x) (cdr x))
                 0
                 1))
         (list 'a 'b 'a 'c 'd 'b 'c))    ; =>  (0 0 1 0 1 1 1)
