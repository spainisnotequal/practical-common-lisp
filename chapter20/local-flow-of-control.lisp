;;; ---------------------------------------------------------------
;;; Use of TAGBODY and GO to locally control the flow of a function
;;; ---------------------------------------------------------------

;; Literal translation of the Algorithm S, as described in page 142, Volume 2,
;; of "The Art of Computer Programming", by Donald Knuth
(defun algorithm-s (n max) ; max is N in Knuth's algorithm
  (let (seen               ; t in Knuth's algorithm
        selected           ; m in Knuth's algorithm
        u                  ; U in Knuth's algorithm
        (records ()))      ; the list where we save the records selected
    (tagbody
     s1
       (setf seen 0)
       (setf selected 0)
     s2
       (setf u (random 1.0))
     s3
       (when (>= (* (- max seen) u) (- n selected)) (go s5))
     s4
       (push seen records)
       (incf selected)
       (incf seen)
       (if (< selected n)
           (go s2)
           (return-from algorithm-s (nreverse records)))
     s5
       (incf seen)
       (go s2))))

;; we use a consistent random seed (bind to *RANDOM-STATE*) just to be sure that
;; if we refactor the function ALGORITHM-S, we can compare results
(let ((*random-state* (make-random-state nil)))
  (algorithm-s 10 200)) ; => (38 54 59 82 114 119 131 150 160 164)

;; Refactor of Algorithm S using more common Lisp expressions
(defun algorithm-s (n max)
  (loop for seen from 0
     when (< (* (- max seen) (random 1.0)) n)
     collect seen and do (decf n)
     until (zerop n)))

;; test the refactoring, using the same random seed
(let ((*random-state* (make-random-state nil)))
  (algorithm-s 10 200)) ; => (38 54 59 82 114 119 131 150 160 164)

