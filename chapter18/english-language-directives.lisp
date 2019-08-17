;;; Numbers as English words:
(format nil "~r" 1234)  ; => "one thousand two hundred thirty-four"

;;; Numbers as ordinals:
(format nil "~:r" 1)    ; => "first"
(format nil "~:r" 2)    ; => "second"
(format nil "~:r" 12)   ; => "twelfth"
(format nil "~:r" 1234) ; => "one thousand two hundred thirty-fourth"

;;; Numbers as Roman numerals:
(format nil "~@r" 1234) ; => "MCCXXXIV"

;;; Plurals:
(format nil "file~p" 1)        ; => "file"
(format nil "file~p" 10)       ; => "files"
(format nil "file~p" 0)        ; => "files"

(format nil "~r file~:p" 1)    ; => "one file"
(format nil "~r file~:p" 10)   ; => "ten files"
(format nil "~r file~:p" 0)    ; => "zero files"

(format nil "~r famil~:@p" 1)  ; => "one family"
(format nil "~r famil~:@p" 10) ; => "ten families"
(format nil "~r famil~:@p" 0)  ; => "zero families"

;;; Capitalize words or phrases:
(format nil "~(~a~)" "anyTHing")   ; => "anything"
(format nil "~@(~a~)" "anyTHing")  ; => "Anything"
(format nil "~:(~a~)" "anyTHing")  ; => "Anything"
(format nil "~:@(~a~)" "anyTHing") ; => "ANYTHING"

(format nil "~(~a~)" "asK Me anyTHing")   ; => "ask me anything"
(format nil "~@(~a~)" "asK Me anyTHing")  ; => "Ask me anything"
(format nil "~:(~a~)" "asK Me anyTHing")  ; => "Ask Me Anything"
(format nil "~:@(~a~)" "asK Me anyTHing") ; => "ASK ME ANYTHING"
