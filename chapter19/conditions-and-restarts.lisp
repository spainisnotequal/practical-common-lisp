;;; ------------------------
;;; Define a condition class
;;; ------------------------
(define-condition malformed-log-entry-error (error)
  ((text :initarg :text
         :reader text)))

;;; -------------------------------------
;;; Signal a condition in PARSE-LOG-ENTRY
;;; -------------------------------------
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
    (make-instance 'log-entry ...)
    (error 'malformed-log-entry-error :text text)))

;;; -------------------------------------
;;; Establish a restart in PARSE-LOG-FILE
;;; -------------------------------------
(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
       for entry = (restart-case (parse-log-entry text)
                     (skip-log-entry () nil))
       when entry collect it)))

;;; ----------------------------------
;;; Handle the restart in LOG-ANALYZER
;;; ----------------------------------
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (dolist (log (find-all-logs))
      (analyze-log log))))

;; SKIP-LOG-ENTRY is the restart function (the function that handles what a
;; specific restart does when we select it):
(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart restart))))

;;; ---------------------------
;;; Providing multiple restarts
;;; ---------------------------
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
    (make-instance 'log-entry ...)
    (restart-case (error 'malformed-log-entry-error :text text)
      (use-value (value) value)
      (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
                  #'(lambda (c)
                      (use-value
                       (make-instance 'malformed-log-entry :text (text c))))))
    (dolist (log (find-all-logs))
      (analyze-log log))))
