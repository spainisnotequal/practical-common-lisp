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

;;; Strings in binary files

(defconstant +null+ (code-char 0))

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
       until (char= char +null+) do (write-char char s))))

(defun write-null-terminated-ascii (string out)
  (loop for char across string
     do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))

;;; Composite structures

(defclass id3-tag    ()
  ((identifier    :initarg :identifier    :accessor    identifier)
   (major-version :initarg :major-version :accessor    major-version)
   (revision      :initarg :revision      :accessor    revision)
   (flags         :initarg :flags         :accessor    flags)
   (size          :initarg :size          :accessor    size)
   (frames        :initarg :frames        :accessor    frames))) 

(defun read-id3-tag (in)
  (let ((tag (make-instance 'id3-tag)))
    (with-slots (identifier major-version revision flags size frames) tag
      (setf identifier    (read-iso-8859-1-string in :length 3))
      (setf major-version (read-u1 in))
      (setf revision      (read-u1 in))
      (setf flags         (read-u1 in))
      (setf size          (read-id3-encoded-size in))
      (setf frames        (read-id3-frames in :tag-size size)))
    tag))

;;; Designing the macros

(define-binary-class
    id3-tag
    ((file-identifier (iso-8859-1-string :length 3))
     (major-version   u1)
     (revision        u1)
     (flags           u1)
     (size            id3-tag-size)
     (frames          (id3-frames :tag-size size))))

;;; Making the dream a reality

;; Bring the with-gensyms macro from Chapter 8
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; Bring the once-only macro from Chapter 8
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

;; translate a symbol to the corresponding keyword symbol
(defun as-keyword (sym)
  (intern (string sym) :keyword))

(as-keyword 'book) ;=> :BOOK, NIL

;; Create a DEFCLASS slot specifier
(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(slot->defclass-slot '(major-version u1))
;=> (MAJOR-VERSION :INITARG :MAJOR-VERSION :ACCESSOR MAJOR-VERSION)
