;;; ----------------------------------------------------
;;; Use of UNWIND-PROTECT to close streams automatically
;;; ----------------------------------------------------

;; Macro to open and close a database connection automatically
(defmacro with-database-connection ((var &rest open-args) &body body)
  `(let ((,var (open-connection ,@open-args)))
    (unwind-protect (progn ,@body)
      (close-connection ,var))))

;; Example of use of that macro
(with-database-connection (conn :host "foo" :user "scott" :password "tiger")
  (do-stuff conn)
  (do-more-stuff conn))
