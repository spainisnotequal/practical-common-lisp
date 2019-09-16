;;; ---------------------------------------------------------------------
;;; Examples of use of LOOP to acummulate numbers, and their alternatives
;;; ---------------------------------------------------------------------

(defparameter *dataset* '(6 6 0 1 0 1 8 4 3 5 5))

;; First example
;; -------------

(defun stats (alist)
  (loop for item in alist
     minimize item into min-value
     maximize item into max-value
     count item into list-count
     sum item into list-sum
     finally (return (values min-value max-value
                             list-count list-sum))))

(stats *dataset*) ; => 0 8 11 39

(defun stats2 (alist)
  (values (reduce #'min alist)
          (reduce #'max alist)
          (list-length alist)
          (reduce #'+ alist)))

(stats2 *dataset*) ; => 0 8 11 39

;;; Second example
;;; --------------

(defun sum-even-and-odd (alist)
  (loop for i in alist
     if (evenp i) sum i into even-sum
     else sum i into odd-sum
     end
     finally (return (values even-sum odd-sum))))

(sum-even-and-odd *dataset*) ; => 24, 15

(defun sum-even-and-odd2 (alist)
  (values (reduce #'+ (remove-if-not #'evenp alist))
          (reduce #'+ (remove-if-not #'oddp alist))))

(sum-even-and-odd2 *dataset*) ; => 24, 15