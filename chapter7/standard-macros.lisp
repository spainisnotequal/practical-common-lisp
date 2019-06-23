;;;; -------------------------------- ;;;;
;;;; Macros predefined in Common Lips ;;;;
;;;; -------------------------------- ;;;;

;;; ---------------
;;; WHEN and UNLESS
;;; ---------------

(defparameter x 1)

;; To execute more than one expression in an IF expression, we need to use the
;; special operator PROGN
(if (equal x 1)
    (progn
      (format t "First message~%")
      (format t "Second message~%")))

;; The macro WHEN captures the pattern of an IF plus a PROGN
(when (equal x 1)
  (format t "First message~%")
  (format t "Second message~%"))

(unless (equal x 0)
  (format t "First message~%")
  (format t "Second message~%"))

;; WHEN and UNLESS are standard macros, but if they weren't part of the standard
;; library, we could write them as follow
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

(my-when (equal x 1)
  (format t "First message~%")
  (format t "Second message~%"))

(my-unless (equal x 0)
  (format t "First message~%")
  (format t "Second message~%"))
