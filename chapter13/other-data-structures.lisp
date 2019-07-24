;;;; --------------------- ;;;;
;;;; OTHER DATA STRUCTURES ;;;;
;;;; --------------------- ;;;;

;;; -----
;;; Lists
;;; -----

;; Difference between a "dotted list" and a "proper list"
(cons 1 (cons 2 (cons 3 4)))            ; => (1 2 3 . 4)
(cons 1 (cons 2 (cons 3 (cons 4 nil)))) ; =>(1 2 3 4)

;;; -----
;;; Trees
;;; -----

(defparameter *l1* (list 1 2))
(defparameter *l2* (list 3 4))
(defparameter *l3* (list 5 6))

(defparameter *t1* (list *l1* *l2* *l3*))

(defparameter *t2* (copy-tree *t1*))

(tree-equal *t1* *t2*) ; => T

(setf (first *l3*) 1)

*t1* ; => ((1 2) (3 4) (1 6))
*t2* ; => ((1 2) (3 4) (5 6))

(tree-equal *t1* *t2*) ; => NIL

(subst -1 1 *t1*) ; => ((-1 2) (3 4) (-1 6))
*t1*              ; => ((1 2) (3 4) (1 6))

(subst-if nil #'oddp *t1*) ; => ; Evaluation aborted on #<TYPE-ERROR expected-type: INTEGER datum: ((1 2) (3 4) (1 6))>.

;; the previous evaluation produces an error because we are passing to the
;; whole tree ((1 2) (3 4) (1 6)) to the predicate ODDP, which expects a number,
;; not a list.
;; The reason is that the cons cells of the tree are processed by SUSBT-IF recursively, first their CAR and later their CONS. So the elements that are
;; passed to ODDP are, in order:
;;     ((1 2) (3 4) (5 6)) 
;;     (1 2) 
;;     1 
;;     (2) 
;;     2 
;;     ((3 4) (5 6)) 
;;     (3 4) 
;;     3 
;;     (4) 
;;     4 
;;     ((5 6)) 
;;     (5 6) 
;;     5 
;;     (6) 
;;     6
;; To process that tree correctly, we should prevent passing a list to ODDP (this
;; function only accepts INTEGERS). We could do that in this way:
(subst-if 'yeah
          #'(lambda (element)
              (when (integerp element)
                (oddp element)))
          *t1*)                        ; => ((YEAH 2) (YEAH 4) (YEAH 6))

;; In the next example, firstly we evaluate (LISTP *t1*), which happens to be T,
;; so we substitute the whole tree by 'YEAH, and, because 'YEAH it's not a cons
;; cell, SUBST-IF ends.
(subst-if 'yeah #'listp *t1*); => YEAH

;;; ----
;;; Sets
;;; ----

;; A set it's just a list, so we can creat an empty set with:
(defparameter *set* '()) ; => NIL

;; ADJOIN does not modify the original set
(adjoin 1 *set*) ; => (1)
*set*            ; => NIL

;; If we want to modify the original set, we should use SETF
(setf *set* (adjoin 1 *set*)) ; => (1)
*set*                         ; => (1)
(setf *set* (adjoin 2 *set*)) ; => (2 1)
*set*                         ; => (2 1)

;; Or use the macro PUSHNEW, that abstracts the SETF/ADJOIN combination
(pushnew 3 *set*) ; => (3 2 1)
*set*             ; => (3 2 1)

;; If the item is found, ADJOIN just returns the original set
(setf *set* (adjoin 1 *set*)) ; => (3 2 1)
*set*                         ; => (3 2 1)

;; To find if an item is in a set, we use MEMBER, MEMBER-IF, or MEMBER-IF-NOT
(member 1 *set*) ; => (1
(member 4 *set*) ; => NIL

(member-if #'(lambda (x) (< x 3)) *set*) ; => (2 1)
(member-if #'(lambda (x) (< x 9)) *set*) ; => (3 2 1)
(member-if #'(lambda (x) (< x 0)) *set*) ; => NIL

;; Some set operations (the items of the resulting set might be in any order)
(defparameter *s1* (list 1 2 3 4))
(defparameter *s2* (list 1 3))

(intersection *s1* *s2*) ; => (3 1)
(union *s1* *s2*) ; => (4 2 1 3)

(subsetp *s2* *s1*) ; => T
(subsetp *s1* *s2*) ; => NIL

;;; -------------
;;; Lookup tables
;;; -------------

;; alists (association lists)
;; --------------------------

;; define an alist, and add a new key/value pair
(defparameter *alist* (list (cons 'a 1) (cons 'b 2)))
*alist*                             ; => ((A . 1) (B . 2))
(setf *alist* (acons 'c 3 *alist*)) ; => ((C . 3) (A . 1) (B . 2))
*alist*                             ; => ((C . 3) (A . 1) (B . 2))

;; define an alist with two lists: a keys list and a values list
(defparameter *alist-1* (pairlis (list 'a 'b 'c) (list 1 2 3)))
*alist-1* ; => ((C . 3) (A . 1) (B . 2))

;; get a key/value pair, or just the value
(assoc 'a *alist*) ; => (A . 1)
(assoc 'c *alist*) ; => (C . 3)
(assoc 'd *alist*) ; => NIL

(assoc-if #'(lambda (symbol) (equal symbol 'a)) *alist*) ; => (A . 1)

(cdr (assoc 'a *alist*)) ; => 1

;; we can provide a test function (for example, if we want to use strings as keys)
(defparameter *alist-2* (list (cons "one" 1) (cons "two" 2) (cons "three" 3)))
*alist-2* : => (("one" . 1) ("two" . 2) ("three" . 3))
(assoc "one" *alist-2*)                 ; => NIL
(assoc "one" *alist-2* :test #'string=) ; => ("one" . 1)

;; copy an alist
(defparameter *alist-3* (copy-alist *alist*))
(setf *alist* (acons 'd 4 *alist*)) ; => ((D . 4) (C . 3) (A . 1) (B . 2))
*alist-3*                           ; => ((C . 3) (A . 1) (B . 2))

;; plists (property lists)
;; -----------------------

;; define an plist
(defparameter *plist* (list 'a 1 'b 2))
*plist* ; => (A 1 B 2)

;; get the value associated to a key
(getf *plist* 'a) ; => 1
(getf *plist* 'c) ; => NIL

;; never use numbers or strings as keys in a plist, because GETF always uses EQ
;; to test whether the provided key matches the keys in the plist
(defparameter *plist-1* (list "a" 1 "b" 2))
(getf *plist-1* "a") ; => NIL
(defparameter *plist-2* (list 1 "a" 2 "b"))
(getf *plist-2* 1) ; => "a" ; in this case, we get the right value, but this
                            ; behaviour depends on the implementation (in SBCL
                            ; it works, but maybe in other implementation it
                            ; won't)

;; add a new key/value pair to a plist
(setf (getf *plist* 'c) 3) ; => 3
*plist*                    ; => (C 3 A 1 B 2)

;; modify a value in a plist
(setf (getf *plist* 'c) 0) ; => 0
*plist*                    ; => (C 0 A 1 B 2)

;; remove a key/value pair from a plist
(remf *plist* 'a) ; => T
*plist*           ; => (C 0 B 2)

;; get the properties of a plist
(defparameter *plist-3* (list 'name "Jesus" 'surname "Christ" 'age 33))
(defun print-properties (plist keys)
  (loop while plist do
       (multiple-value-bind (key value tail) (get-properties plist keys)
         (when key (format t "key: ~a; value: ~a~%" key value))
         (setf plist (cddr tail)))))
(print-properties *plist-3* (list 'name 'surname 'age))
                                        ; => key: NAME; value: Jesus
                                        ;    key: SURNAME; value: Christ
                                        ;    key: AGE; value: 33
                                        ;    NIL

(print-properties *plist-3* (list 'name 'age))
                                        ; => key: NAME; value: Jesus
                                        ;    key: AGE; value: 33
                                        ;    NIL

;; every symbol has an associate plist that can be used to store information
;; about the symbol. This is useful in some situations (like when doing any kind
;; of symbolic programming).

(defparameter *symbol* 'Frank)      ; => *SYMBOL*
(symbol-plist *symbol*)             ; => NIL

(setf (get *symbol* 'job) "hacker") ; => "hacker"
(setf (get *symbol* 'fav-numbers)
      '(1 3 5 7 9))                 ; => (1 3 5 7 9)
(symbol-plist *symbol*)             ; => (FAV-NUMBERS (1 3 5 7 9) JOB "hacker")

(remprop *symbol* 'fav-numbers)     ; => (FAV-NUMBERS (1 3 5 7 9) JOB "hacker")
(symbol-plist *symbol*)             ; => (JOB "hacker")
(remprop *symbol* 'job)             ; => (JOB "hacker")
(symbol-plist *symbol*)             ; => NIL

;;; ------------------------
;;; Destructuring-bind macro
;;; ------------------------

;; Destructuring a simple list:
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z)) ; => (:X 1 :Y 2 :Z 3)

;; Destructuring a list that contains another list:
(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z)) ; => (:X 1 :Y (2 20) :Z 3)
(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; => (:X 1 :Y1 2 :Y2 20 :Z 3)

;; Using the &optional parameter:
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; => (:X 1 :Y1 2 :Y2 20 :Z 3)
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; => (:X 1 :Y1 2 :Y2 NIL :Z 3)

;; Using the &key parameter:
(destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
  (list :x x :y y :z z)) ; => (:X 1 :Y 2 :Z 3)
(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z)) ; => (:X 3 :Y 2 :Z 1)

;; Using the &whole parameter:
(destructuring-bind (&whole whole &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole)) ; => (:X 3 :Y 2 :Z 1 :WHOLE (:Z 1 :Y 2 :X 3))
