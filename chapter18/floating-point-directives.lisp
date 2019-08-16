(format nil "~f" pi)     ; => "3.141592653589793"
(format nil "~,5f" pi)   ; => "3.14159"
(format nil "~vf" 7 pi)  ; => "3.14159"

(format nil "~e" pi)     ; => "3.141592653589793d+0"
(format nil "~,5e" pi)   ; => "3.14159d+0"
(format nil "~,ve" 5 pi) ; => "3.14159d+0"

(format nil "~$" pi)     ; => "3.14"
(format nil "~v$" 5 pi)  ; => "3.14159"
(format nil "~,5$" pi)   ; => "00003.14"
(format nil "~5,2$" pi)  ; => "03.14159"
