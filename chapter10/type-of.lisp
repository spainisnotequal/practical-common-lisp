;;;; ---------------- ;;;;
;;;; TYPE-OF function ;;;;
;;;; ---------------- ;;;;

(type-of 1)        ; => BIT
(type-of 2)        ; => (INTEGER 0 536870911)
(type-of 1/2)      ; => RATIO
(type-of 3.14159)  ; => SINGLE_FLOAT

(type-of #\a)      ; => STANDARD-CHAR
(type-of 'foo)     ; => SYMBOL
(type-of "foo")    ; => (SIMPLE-ARRAY CHARACTER (3))

(type-of (list 1 2 3)) ; => CONS
(type-of (vector 1 2 3)) ; => (SIMPLE-VECTOR 3)
