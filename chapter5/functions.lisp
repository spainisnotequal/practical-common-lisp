;;; ------------------------
;;; Function Parameter Lists
;;; ------------------------

;; -------------------
;; Required parameters
;; -------------------

(defun create-list (a b c)
  (list a b c))

(create-list 1 2 3)   ; => (1 2 3)
(create-list 1 2)     ; => invalid number of arguments: 2
(create-list 1)       ; => invalid number of arguments: 1
(create-list)         ; => invalid number of arguments: 0
(create-list 1 2 3 4) ; => invalid number of arguments: 4

;; -------------------
;; Optional parameters
;; -------------------

(defun create-list (a b &optional (c 0 c-supplied-p))
  (list a b c c-supplied-p))

(create-list 1 2 3 ); => (1 2 3 T)
(create-list 1 2)   ; => (1 2 0 NIL)
(create-list 1)     ; => invalid number of arguments: 1
(create-list)       ; => invalid number of arguments: 0

(defun create-list (a b &optional (c (/ a b)))
  (list a b c))

(create-list 1 2)   ; => (1 2 1/2)
(create-list 1 2 3) ; => (1 2 3)

;; ---------------
;; Rest parameters
;; ---------------

(defun create-list (a b &rest rest)
  (list a b rest))

(create-list 1 2 3) ; => (1 2 (3))
(create-list 1 2 3 4) ; => (1 2 (3 4))

;; ------------------
;; Keyword parameters
;; ------------------

(defun create-list (&key a b c)
  (list a b c))

(create-list)                 ; => (NIL NIL NIL)
(create-list :a 1)            ; => (1 NIL NIL)
(create-list :b 1)            ; => (NIL 1 NIL)
(create-list :c 1)            ; => (NIL NIL 1)
(create-list :a 1 :c 1)       ; => (1 NIL 1)
(create-list :c 1 :b 0)       ; => (NIL 0 1)
(create-list :c 1 :a -6 :b 0) ; => (-6 0 1)

(defun create-list (&key (a 0) (b 0 b-supplied-p) (c 0))
  (list a b c b-supplied-p))

(create-list)                 ; => (0 0 0 NIL)
(create-list :a 1)            ; => (1 0 0 NIL)
(create-list :b 1)            ; => (0 1 0 T)
(create-list :c 1)            ; => (0 0 1 NIL)
(create-list :a 1 :c 1)       ; => (1 0 1 NIL)
(create-list :c 1 :b 0)       ; => (0 0 1 T)
(create-list :c 1 :a -6 :b 0) ; => (-6 0 1 T)
