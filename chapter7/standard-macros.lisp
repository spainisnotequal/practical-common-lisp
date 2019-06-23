;;;; -------------------------------- ;;;;
;;;; Macros predefined in Common Lips ;;;;
;;;; -------------------------------- ;;;;

;; ---------------
;; WHEN and UNLESS
;; ---------------

(defparameter x 1)

(if (equal x 1)
    (progn
      (format t "First message~%")
      (format t "Second message~%")))

(when (equal x 1)
  (format t "First message~%")
  (format t "Second message~%"))

(unless (equal x 0)
  (format t "First message~%")
  (format t "Second message~%"))
