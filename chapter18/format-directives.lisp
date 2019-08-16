(format t "~f" pi)    ; => 3.141592653589793, NIL
(format t "~,5f" pi)  ; => 3.14159, NIL
(format t "~$" pi)    ; => 3.14, NIL
(format t "~5$" pi)   ; => 3.14159, NIL
(format t "~v$" 5 pi) ; => 3.14159, NIL
(format t "~5,2$" pi) ; => 03.14159, NIL

(format t "~d" 1000000)   ; => 1000000, NIL
(format t "~:d" 1000000)  ; => 1,000,000, NIL
(format t "~@d" 1000000)  ; => +1000000, NIL
(format t "~:@d" 1000000) ; => +1,000,000, NIL
