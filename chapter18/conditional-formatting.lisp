;;; Conditional formatting:
(format nil "~[cero~;uno~;dos~]" 0) ; => "cero"
(format nil "~[cero~;uno~;dos~]" 1) ; => "uno"
(format nil "~[cero~;uno~;dos~]" 2) ; => "dos"
(format nil "~[cero~;uno~;dos~]" 3) ; => ""

;;; Default clause:
(format nil "~[cero~;uno~;dos~:;mucho~]" 3)   ; => "mucho"
(format nil "~[cero~;uno~;dos~:;mucho~]" 100) ; => "mucho"

;;; Sequencial process of arguments using "#":
(defparameter *list-etc*
  "~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].")

(format nil *list-etc*)                ; => "NONE."
(format nil *list-etc* 'a)             ; => "A."
(format nil *list-etc* 'a 'b)          ; => "A and B."
(format nil *list-etc* 'a 'b 'c)       ; => "A, B and C."
(format nil *list-etc* 'a 'b 'c 'd)    ; => "A, B, C, etc."
(format nil *list-etc* 'a 'b 'c 'd 'e) ; => "A, B, C, etc."

;;; Conditional arguments using ":":
(format nil "~:[FAIL~;pass~]" (zerop 1)) ; => "FAIL"
(format nil "~:[FAIL~;pass~]" (zerop 0)) ; => "pass"

;;; Several clauses must be written separately:
(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 20)   ; => "x = 10 y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 nil)  ; => "x = 10 "
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil 20)  ; => "y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil nil) ; => ""
