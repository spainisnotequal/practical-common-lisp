;;;; ------------------- ;;;;
;;;; UNIT TEST FRAMEWORK ;;;;
;;;; ------------------- ;;;;

;;; ---------------
;;; Two first tries
;;; ---------------

(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;;; -----------
;;; Refactoring
;;; -----------

;; REPORT-RESULT function
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

;; Third version of TEST-+
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;; CHECK macro
(defmacro check (form)
  `(report-result ,form ',form))

;; Fourth version of TEST-+
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

;; Second version of the CHECK macro
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;; Fifth version of TEST-+
(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

