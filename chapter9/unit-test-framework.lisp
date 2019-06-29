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

;;; -----------------------
;;; Fixing the return value
;;; -----------------------

;; Second version of REPORT-RESULT function
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

;; WITH-GEMSYMS macro (from chapter 8):
(defmacro with-gensyms ((&rest names) &body body)
  "Creates new variables with unique symbols to be used in macro's expansions"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; COMBINE-RESULTS macro
(defmacro combine-results (&body forms)
  "Works like the AND logical operator but running all forms instead of stopping when a forms returns NIL"
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;; Third version of the CHECK macro
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;; TEST-+ that fails the second test
(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 7)
    (= (+ -1 -3) -4)))

;;; -----------------------
;;; Better result reporting
;;; -----------------------

;; Use a dynamic (global) variable to bind which test we are running each time
;; we run REPORT-RESULT from CHECK
(defvar *test-name* nil)

;; Third version of REPORT-RESULT function
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;; TEST-+
(defun test-+ ()
  (let ((*test-name* 'test-+))
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 7)
      (= (+ -1 -3) -4))))

;; TEST-* function
(defun test-* ()
  (let ((*test-name* 'test-*))
    (check
      (= (* 2 2) 4)
      (= (* -3 5) -15))))

;; TEST-ARITHMETIC function
(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

;;; ----------------------
;;; An abstraction emerges
;;; ----------------------

;; Define a macro named DEFTEST that abstracts the creation of test functions
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))

;; TEST-+ function
(deftest test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 7)
   (= (+ -1 -3) -4)))

;; TEST-* function
(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* -3 5) -15)))

;; TEST-ARITHMETIC function
(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

;;; --------------------
;;; A hierarchy of tests
;;; --------------------

;; Second version of DEFTEST to support multiple levels of test functions
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; recompile TEST-+
(deftest test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 7)
   (= (+ -1 -3) -4)))

;; recompile TEST-*
(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* -3 5) -15)))

;; Now define TEST-ARITHMETIC with DEFTEST instead of with DEFUN
(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

;; Add another layer on top of TEST-ARITHMETIC
(deftest test-math ()
  (test-arithmetic))

;;; ------------------------------------------
;;; Wrapping up - Test framework complete code
;;; ------------------------------------------

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
