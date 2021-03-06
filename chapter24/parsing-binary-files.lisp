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

;; First version of DEFINE-BINARY-CLASS
(defmacro define-binary-class (name slots)
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))

(macroexpand-1
 '(define-binary-class
   id3-tag
   ((identifier    (iso-8859-1-string :length 3))
    (major-version u1)
    (revision      u1)
    (flags         u1)
    (size          id3-tag-size)
    (frames        (id3-frames :tag-size size)))))


;; (DEFCLASS ID3-TAG NIL
;;           ((IDENTIFIER    :INITARG :IDENTIFIER    :ACCESSOR IDENTIFIER)
;;            (MAJOR-VERSION :INITARG :MAJOR-VERSION :ACCESSOR MAJOR-VERSION)
;;            (REVISION      :INITARG :REVISION      :ACCESSOR REVISION)
;;            (FLAGS         :INITARG :FLAGS         :ACCESSOR FLAGS)
;;            (SIZE          :INITARG :SIZE          :ACCESSOR SIZE)
;;            (FRAMES        :INITARG :FRAMES        :ACCESSOR FRAMES)))


;;; Reading binary objects

;; READ-VALUE generic function
(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

;; Generate the appopriate SETF form from a slot specifier
(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(slot->read-value '(major-version u1) 'stream)
;=> (SETF MAJOR-VERSION (READ-VALUE 'U1 STREAM))
(slot->read-value '(identifier (iso-8859-1-string :length 3)) 'stream)
;=> (SETF IDENTIFIER (READ-VALUE 'ISO-8859-1-STRING STREAM :LENGTH 3))

;; Add the READ-VALUE method to the DEFINE-BINARY-CLASS macro
(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'first slots) ,objectvar
             ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar)))))

;;; Writing binary objects

;; WRITE-VALUE generic function
(defgeneric write-value (type stream &key)
  (:documentation "Write a value of the given type from the stream."))

;; Generate the appopriate WRITE-VALUE expression from a slot specifier
(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

;;; Add the WRITE-VALUE method to the DEFINE-BINARY-CLASS macro
(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'first slots) ,objectvar
             ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar))

       (defmethod write-value ((,typevar (eql ',name)) ,streamvar ,objectvar &key)
         (with-slots ,(mapcar #'first slots) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;;; Adding inheritance and tagged structures

;; Define generic functions to take an object instead of an object type
(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

;; Redefine the  DEFINE-BINARY-CLASS macro to be able to support inheritance
(defmacro define-binary-class (name superclasses slots)
  (with-gensyms (objectvar streamvar)
    `(progn
       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slots) ,objectvar
                    ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slots) ,objectvar
                    ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;;; Keeping track of inherited slots

;; Redefine the  DEFINE-BINARY-CLASS macro to be able to refer to inherited slots
(defmacro define-binary-class (name superclasses slots)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slots) ,objectvar
                    ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slots) ,objectvar
                    ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;; Access to the object's slots
(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
     nconc (direct-slots super)
     nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

;;; Tagged structures

;; Factor out the common part of DEFINE-BINARY-CLASS AND DEFINE-TAGGED-BINARY-CLASS
(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
                  (declare (ignorable ,streamvar))
                  (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                    ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;; Redefine the DEFINE-BINARY-CLASS macro to use the above DEFINE-GENERIC-BINARY-CLASS macro
(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
                                  (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                                             (declare (ignorable ,streamvar))
                                             (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                                               ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

;; Define the DEFINE-TAGGED-BINARY-CLASS
(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
                                  (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
                                    (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
                                      (let ((,objectvar
                                             (make-instance
                                              ,@(or (cdr (assoc :dispatch options))
                                                    (error "Must supply :dispatch form."))
                                              ,@(mapcan #'slot->keyword-arg slots))))
                                        (read-object ,objectvar ,streamvar)
                                        ,objectvar))))))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;;; Primitive binary types

;; Macro to generate READ-VALUE and WRITE-VALUE methods to read values represented by instances
(defmacro define-binary-type (name (&rest args) &body spec)
  (with-gensyms (type)
    `(progn
       ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
          `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
             ,@body))
       
       ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
          `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
             ,@body)))))

;; Redefine the DEFINE-BINARY-TYPE macro to support two ways of calling it
(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
         `(progn
            (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
              (read-value ',derived-from ,stream ,@derived-args))
            (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
              (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                ,@body))
          ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
             `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
                ,@body)))))))

