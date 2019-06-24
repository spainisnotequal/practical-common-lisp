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


;;; ----------------------------------
;;; Multibranch conditionals with COND
;;; ----------------------------------

(defparameter s 'o)

;; Multibranch conditionals using IF are not easy to read
(if (string-equal s 'a)
    (format t "First vowel~%")
    (if (string-equal s 'e)
        (format t "Second vowel~%")
        (if (string-equal s 'i)
            (format t "Third vowel~%")
            (if (string-equal s 'o)
                (format t "Fourth vowel~%")
                (if (string-equal s 'u)
                    (format t "Fifth vowel~%")
                    (format t "Not a vowel~%"))))))

;; The macro COND abstracts that idea of multibranch conditionals
(cond
  ((string-equal s 'a) (format t "First vowel~%"))
  ((string-equal s 'e) (format t "Second vowel~%"))
  ((string-equal s 'i) (format t "Third vowel~%"))
  ((string-equal s 'o) (format t "Fourth vowel~%"))
  ((string-equal s 'u) (format t "Fifth vowel~%"))
  (t (format t "Not a vowel~%")))
