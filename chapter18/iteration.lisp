;;; Iteration directive "{":
(format nil "~{~a, ~}" (list 1 2 3))   ; => "1, 2, 3, "
(format nil "~{~a~^, ~}" (list 1 2 3)) ; => "1, 2, 3"
(format nil "~@{~a~^, ~}" 1 2 3)       ; => "1, 2, 3"

;;; Number of items to be processed:
(format nil "~{~a~#[~;, and ~:;, ~]~}" (list 1 2 3)) ; => "1, 2, and 3"
(format nil "~{~a~#[~;, and ~:;, ~]~}" (list 1 2))   ; => "1, and 2"

(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(format nil *english-list* '())        ; => ""
(format nil *english-list* '(1))       ; => "1"
(format nil *english-list* '(1 2))     ; => "1 and 2"
(format nil *english-list* '(1 2 3))   ; => "1, 2, and 3"
(format nil *english-list* '(1 2 3 4)) ; => "1, 2, 3, and 4"

;;; Force at least one iteration:
(defparameter *english-list*
  "~{~#[<empty>~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~:}")

(format nil *english-list* '()) ; => "<empty>"
