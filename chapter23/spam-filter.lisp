(in-package :spam)

;;; ------------------------
;;; CLASSIFY (main function)
;;; ------------------------

(defun classify (text)
  (classification (score (extract-features text))))

;;; -----------------------
;;; CLASSIFICATION function
;;; -----------------------

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
  (cond
    ((<= score *max-ham-score*)  'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))
