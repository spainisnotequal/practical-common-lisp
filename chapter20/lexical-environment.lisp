;;; --------------------------------------------------------------------
;;; Example of a recursive function defined in the local scope of LABELS
;;; --------------------------------------------------------------------

(labels ((factorial (n &optional (result 1))
           (if (<= n 1)
               result
               (factorial (1- n) (* result n)))))
  (print (factorial 5)))

;;; -------------------------------------------------------------------
;;; Recursive helper function WALK, in the local scope of COLLECT-TREES
;;; -------------------------------------------------------------------

(defun collect-leaves (tree)
  (let ((leaves ()))
    (labels ((walk (tree)
               (cond
                 ((null tree))
                 ((atom tree) (push tree leaves))
                 (t (walk (car tree))
                    (walk (cdr tree))))))
      (walk tree))
    (nreverse leaves)))

(defparameter *tree* (list (list 1 2) 3 (list 4 5)))
*tree*                  ; => ((1 2) 3 (4 5))
(collect-leaves *tree*) ; => (1 2 3 4 5)
