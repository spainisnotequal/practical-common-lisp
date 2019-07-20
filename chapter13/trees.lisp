;;;; ----- ;;;;
;;;; TREES ;;;;
;;;; ----- ;;;;

;;; -------------------------------
;;; Defining trees using cons cells
;;; -------------------------------

;; 1 node
(defparameter *t1*  (cons 1 nil)) ; => (1)

;; 2 nodes
(defparameter *t2* (cons (cons 1 nil) (cons (cons 2 nil) nil))) ; => ((1) (2))

;; 3 nodes
(defparameter *t3* (cons (cons 1 nil)
                         (cons (cons 2 nil)
                               (cons (cons 3 nil)
                                     nil))))       ; => ((1) (2) (3))

;; 4 nodes
(defparameter *t4* (cons (cons 3 nil) (cons (cons 4 nil) nil))) ; => ((3) (4))

(defparameter *t5* (cons *t2* (cons *t4* nil))) ; => (((1) (2)) ((3) (4)))
(defparameter *t6* (cons *t2* (cons *t3* nil))) ; => (((1) (2)) ((1) (2) (3)))

;;; -----------------------------
;;; Visiting every leaf of a tree
;;; -----------------------------

(defun map-tree (f tree)
  (mapcar #'(lambda (sub-tree)
              (if (consp sub-tree)
                  (map-tree f sub-tree)
                  (funcall f sub-tree)))
          tree))

(defun square (x)
  (* x x))

(map-tree #'square *t1*) ; => (1)
(map-tree #'square *t2*) ; => ((1) (4))
(map-tree #'square *t3*) ; => ((1) (4) (9))
(map-tree #'square *t5*) ; => (((1) (4)) ((9) (16)))
(map-tree #'square *t6*) ; => (((1) (4)) ((1) (4) (9)))
