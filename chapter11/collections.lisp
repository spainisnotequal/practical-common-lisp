;;;; ----------- ;;;;
;;;; COLLECTIONS ;;;;
;;;; ----------- ;;;;

;;; -------------------------
;;; Create vectors and arrays
;;; -------------------------

(vector)     ; -> #()
(vector 1)   ; => #(1)
(vector 1 2) ; => #(1 2)

(make-array 4) ; => #(0 0 0 0)
(make-array 4 :initial-element nil) ; => #(NIL NIL NIL NIL)
(make-array 4 :fill-pointer 0) ; => #()
(make-array 4 :fill-pointer 2) ; => #(0 0) 

;;; ----------------------
;;; Add or delete elements
;;; ----------------------

(defparameter *v* (make-array 3 :fill-pointer 2)) ; => *V*
*v*                  ; => #(0 0)
(vector-push 33 *v*) ; =>2
*v*                  ; => #(0 0 33)
(vector-push 33 *v*) ; => NIL
*v*                  ; => #(0 0 33)
(vector-pop *v*)     ; => 33
*v*                  ; => #(0 0)
(vector-pop *v*)     ; => 0
*v*                  ; => #(0)
(vector-pop *v*)     ; => 0
*v*                  ; => #()
(vector-pop *v*)     ; => ERROR - "There is nothing left to pop."

;;; --------------------
;;; Length of a sequence
;;; --------------------

(defparameter *v* (vector -3 9 5))

(length *v*) ; => 3

;;; ------------------------------------
;;; Access to the elements of a sequence
;;; ------------------------------------

(defparameter *v* (vector -3 9 5))

(elt *v* 0) ; => -3
(elt *v* 1) ; => 9
(elt *v* 2) ; => 5
(elt *v* 3) ; => ERROR - "The index 3 is too large."

;; ELT is a SETFable place, so you can set the value of a particular element
;; like this:
(setf (elt *v* 1) -11) ; => -11
*v*                    ; => #(-3 -11 5)

;;; -----------------------------------------------------
;;; Finding and filtering specific elements of a sequence
;;; -----------------------------------------------------

(count 1 '(1 2 3 1 2 3)) ; => 2

(remove 1 #(1 2 3 1 2 3)) ; => #(2 3 2 3)
(remove 1 '(1 2 3 1 2 3)) ; => (2 3 2 3)
(remove #\a "foobarbaz")  ; => "foobrbz"

(substitute 9 1 #(1 2 3 1 2 3))  ; => #(9 2 3 9 2 3)
(substitute 9 1 '(1 2 3 1 2 3))  ; => (9 2 3 9 2 3)
(substitute #\A #\a "foobarbaz") ; => "foobArbAz"

(find 1 #(1 2 3 1 2 3)) ; => 1
(find 1 '(1 2 3 1 2 3)) ; => 1
(find #\a "foobarbaz")  ; => #\a

(position 1 '(1 2 3 1 2 3)) ; => 0
(position #\a "foobarbaz")  ; => 4

;; We can use keyword arguments to modify the behaviour of these functions

;; :test keyword
(remove 2 '(1 2 3 1 2 3)
        :test #'>=)       ; => (3 3)
(remove 2 '(1 2 3 1 2 3)
        :test #'(lambda (x y) (>= x y))) ; => (3 3)

(remove 2 '(1 2 3 1 2 3)
        :test (complement #'>=)) ; => (1 2 1 2)

;; :key keyword
(remove 2 '((1 "one") (2 "two") (3 "three"))
        :key #'first)                        ; => ((1 "one") (3 "three"))
(remove 2 '((1 "one") (2 "two") (3 "three"))
        :test #'>=
        :key #'first)                        ; => (3 "three")

;; :from-end keyword
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)             ; => (A 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; => (A 30)

;;; --------------------------------------------------------------
;;; Higher-Order variants of these finding and filtering functions
;;; --------------------------------------------------------------

(count-if-not #'evenp '(1 2 3 1 2 3)) ; => 4
(count-if #'evenp '(1 2 3 1 2 3))     ; => 2

(remove-if-not #'digit-char-p "user1234") ; => "1234"
(remove-if #'digit-char-p "user1234")     ; => "user" 

(substitute-if-not #\*
                   #'(lambda (char)
                       (find char "aeiou" :test #'char-equal))
                   "Spain") ; => "**ai*"
(substitute-if #\*
               #'(lambda (char)
                   (find char "aeiou" :test #'char-equal))
               "Spain")     ; => "Sp**n"

(find-if-not #'(lambda (x) (>= x 3))
             #(1 2 3 1 2 3)) ; => 1
(find-if #'(lambda (x) (>= x 3))
         #(1 2 3 1 2 3))     ; => 3

(position-if-not #'evenp '(1 2 3 1 2 3)) ; => 0
(position-if #'evenp '(1 2 3 1 2 3))     ; => 1

;; REMOVE-IF-NOT is a very common and useful function, that returns a copy of
;; sequence with elements not satisfying PREDICATE removed.
;; (It's like FILTER in Python, or GREP in Perl.)

(remove-if-not #'digit-char-p "user1234") ; => "1234"

(remove-if-not #'(lambda (char)
                   (find char "aeiou" :test #'char-equal))
               "Keep the vowels in this text") ; =>   "eeeoeiie"

;; The REMOVE family of functions also support a fourth variant,
;; REMOVE-DUPLICATES, that removes all but one instance of each duplicated
;; element of the given sequence
(remove-duplicates '(1 2 1 2 3 1 2 3 4)) ; => (1 2 3 4)

;;; -----------------------------------
;;; Other useful sequence manipulations
;;; -----------------------------------

(copy-seq '(1 2 3)) ; => (1 2 3)
(reverse '(1 2 3))  ; => (3 2 1)

(concatenate 'list '(1 2 3) '(4 5 6))      ; => (1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6))      ; => (1 2 3 4 5 6)

(concatenate 'vector #(1 2 3) '(4 5 6))    ; => #(1 2 3 4 5 6)
(concatenate 'vector #(1 2 3) #(4 5 6))    ; => #(1 2 3 4 5 6)

(concatenate 'string "abc" "DEF")          ; => "abcDEF"
(concatenate 'string "abc" '(#\D #\E #\F)) ; => "abcDEF"

;;; -------------------------------------------------------
;;; Sorting functions (be careful, as they are destructive)
;;; -------------------------------------------------------

(defparameter my-seq-1 '(2 1 5 3 4))
(defparameter my-seq-2 '(2 1 5 3 4))

;; SORT (and STABLE-SORT) modifies the original sequence
(sort my-seq-1 #'<) ; => (1 2 3 4 5)
(print my-seq-1)    ; => (1 2 3 4 5)

;; to avoid the modification of the original sequence, use COPY-SEQ
(sort (copy-seq my-seq-2) #'<) ; => (1 2 3 4 5)
(print my-seq-2)               ; => (2 1 5 3 4)

;; SORT-STABLE guarantees not to reorder any elements cosidered equivalent by
;; the predicate, while SORT guarantees only that the result is sorted (it may
;; reorder equivalent elements)
(stable-sort my-seq-1 #'<) ; => (1 2 3 4 5)
(print my-seq-1)           ; => (1 2 3 4 5)

;; A very important question: if you want your variable to contain the proper
;; value of the sorted sequence after sorting a sequence, it is necessary to do
;; the assignment. If you don't care about that and only want the return value
;; of sort, you don't need an assignment.
;; So it is a good practise to always do something with the return value of a
;; sorting funtion since these functions can destroy the sequence.
;;
;; There are two reasons for this. First, an implementation is allowed to use
;; non-destructive copying to implement destructive operations. Secondly,
;; destructive operations on lists can permute the conses such that the value
;; passed into the operation no longer points to the first cons of the sequence.
;;
;; Here's an example of the second problem (run under SBCL):
(let ((xs (list 4 3 2 1)))
  (sort xs #'<)
  xs)                     ; => (3 4)

;; In SBCL, we get the following warning:
;;   caught STYLE-WARNING:
;;     The return value of STABLE-SORT-LIST should not be discarded.

;; So to avoid this problem, we need to do the assignment:
(let ((xs (list 4 3 2 1)))
  (setf xs (sort xs #'<))
  xs)                     ; => (1 2 3 4)

;;; -------------------
;;; Merge two sequences
;;; -------------------

(merge 'vector #(1 3 5) #(2 4 6) #'<) ; => #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<)   ; => (1 2 3 4 5 6)

;;; -----------------------
;;; Sub-sequence operations
;;; -----------------------

;; SUBSEQ
(subseq "newspaperman" 9)   ; => "man"
(subseq "newspaperman" 4 9) ; => "paper"
(subseq "newspaperman" 0 4) ; => "news"

(subseq '(1 2 3 4) 3)   ; => (4)
(subseq '(1 2 3 4) 1 3) ; => (2 3)

;; POSITION
(position 'man '(news paper man)) ; => 2
(position 'men '(news paper man)) ; => NIL

(position #\w "newspaperman") ; => 2
(position #\z "newspaperman") ; => NIL

(position 9 #(5 9 1 2)) ; => 1
(position 8 #(5 9 1 2)) ; => NIL

;;SEARCH
(search '(man) '(news paper man)) ; => 2
(search '(men) '(news paper man)) ; => NIL

(search "man" "newspaperman") ; => 9
(search "men" "newspaperman") ; => NIL

(search #(9) #(5 9 1 2)) ; => 1
(search #(8) #(5 9 1 2)) ; => NIL

;; MISMATCH
(mismatch "newspaperman" "newton") ; => 3
(mismatch "newspaperman" "paperman") ; => 0
(mismatch "newspaperman" "paperman" :from-end t) ; => 4

;; MEMBER (only works for lists)
(member 9 '(5 9 1 2)) ; => (9 1 2) 
(member 8 '(5 9 1 2)) ; => NIL

;;; -------------------
;;; Sequence predicates
;;; -------------------

;; One sequence only
(every #'evenp '(1 2 3 4))    ; => NIL
(some #'evenp '(1 2 3 4))     ; => T
(notany #'evenp '(1 2 3 4))   ; => NIL
(notevery #'evenp '(1 2 3 4)) ; => T

;; Two sequences
(every #'= '(1 2 3 4) '(5 4 3 2))    ; => NIL
(some #'= '(1 2 3 4) '(5 4 3 2))     ; => T
(notany #'= '(1 2 3 4) '(5 4 3 2))   ; => NIL
(notevery #'= '(1 2 3 4) '(5 4 3 2)) ; => T
