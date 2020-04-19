;;; Binary format basics

(defun read-u2 (in)
  (+ (* (read-byte in) 256) (read-byte in)))


(ldb (byte 8 0) #xabcd)
(ldb (byte 8 8) #xabcd)

(defvar *num* 0)
(setf (ldb (byte 8 0) *num*) 128)
(setf (ldb (byte 8 8) *num*) 255)

(defun read-u2 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))

(defun write-u2 (out value)
  (write-byte (ldb (byte 8 8) value) out)
  (write-byte (ldb (byte 8 0) value) out))
