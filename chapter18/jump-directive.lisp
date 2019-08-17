;;; Jump using "~*":
(format nil "~{~s~*~^ ~}" '(:a 10 :b 20)) ==> ":A :B"
