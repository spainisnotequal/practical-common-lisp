;; For the user (in a human-readable form):
(format nil "The value is: ~a" 10)           ; => The value is: 10, NIL

(format nil "The value is: ~a" "foobar")     ; => The value is: foobar, NIL

(format nil "The value is: ~a" "\"foobar\"") ; => The value is: "foobar", NIL
(format nil "The value is: ~a" "foo/bar")    ; => The value is: foo/bar, NIL
(format nil "The value is: ~a" "foo\bar")    ; => The value is: foobar, NIL
(format nil "The value is: ~a" "foo\\bar")   ; => The value is: foo\bar, NIL

(format nil "The value is: ~a" '(1 2 3))     ; => The value is: (1 2 3), NIL

;; For the reader (output than can be read back in with READ):
(format nil "~s" 10)         ; => "10"
(format nil "~s" "foobar")   ; => "\"foobar\""
(format nil "~s" 'foobar)    ; => "FOOBAR"
(format nil "~s" '(1 2 3))  ; => "(1 2 3)"

;; Newline and freshline:
(progn
  (format t "~%line1~%")
  (format t "~%line2~%"))
; => 
; => line1
; => 
; => line2
; => NIL

(progn
  (format t "~&line1~&")
  (format t "~&line2~&"))
; => line1
; => line2
; => NIL

(progn
  (format t "~2&line1~2&")
  (format t "~2&line2~2&"))
; => 
; => line1
; => 
; => 
; => line2
; => 
; => NIL
