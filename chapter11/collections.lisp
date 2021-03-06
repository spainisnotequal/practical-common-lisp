;;;; ------- ;;;;
;;;; VECTORS ;;;;
;;;; ------- ;;;;

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

(aref *v* 0) ; => -3
(aref *v* 1) ; => 9
(aref *v* 2) ; => 5
(aref *v* 3) ; => ERROR - "Invalid index 3 for (SIMPLE-VECTOR 3), should be a
             ;             non-negative integer below 3."

;; AREF is a SETFable place, so you can set the value of a particular element
;; like this:
(setf (aref *v* 1) -11) ; => -11
*v*                     ; => #(-3 -11 5)

;;; -----------------------------------------------------
;;; Finding and filtering specific elements of a sequence
;;; -----------------------------------------------------

(count 1 (list 1 2 3 1 2 3)) ; => 2

(remove 1 (vector 1 2 3 1 2 3)) ; => #(2 3 2 3)
(remove 1 (list 1 2 3 1 2 3))   ; => (2 3 2 3)
(remove #\a "foobarbaz")        ; => "foobrbz"

(substitute 9 1 (vector 1 2 3 1 2 3))  ; => #(9 2 3 9 2 3)
(substitute 9 1 (list 1 2 3 1 2 3))    ; => (9 2 3 9 2 3)
(substitute #\A #\a "foobarbaz")       ; => "foobArbAz"

(find 1 (vector 1 2 3 1 2 3)) ; => 1
(find 1 (list 1 2 3 1 2 3))   ; => 1
(find #\a "foobarbaz")        ; => #\a

(position 1 (list 1 2 3 1 2 3)) ; => 0
(position #\a "foobarbaz")      ; => 4

;; We can use keyword arguments to modify the behaviour of these functions

;; :test keyword
(remove 2 (list 1 2 3 1 2 3)
        :test #'>=)                      ; => (3 3)
(remove 2 (list 1 2 3 1 2 3)
        :test #'(lambda (x y) (>= x y))) ; => (3 3)

(remove 2 (list 1 2 3 1 2 3)
        :test (complement #'>=))         ; => (1 2 1 2)

;; :key keyword
(remove 2 (list (list 1 "one")
                (list 2 "two")
                (list 3 "three"))
        :key #'first)             ; => ((1 "one") (3 "three"))
(remove 2 (list (list 1 "one")
                (list 2 "two")
                (list 3 "three"))
        :test #'>=
        :key #'first)             ; => (3 "three")

;; :from-end keyword
(find 'a (vector (list 'a 10)
                 (list 'b 20)
                 (list 'a 30)
                 (list 'b 40))
      :key #'first)             ; => (A 10)
(find 'a (vector (list 'a 10)
                 (list 'b 20)
                 (list 'a 30)
                 (list 'b 40))
      :key #'first :from-end t) ; => (A 30)

;;; --------------------------------------------------------------
;;; Higher-Order variants of these finding and filtering functions
;;; --------------------------------------------------------------

(count-if-not #'evenp (list 1 2 3 1 2 3)) ; => 4
(count-if #'evenp (list 1 2 3 1 2 3))     ; => 2

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
             (vector 1 2 3 1 2 3))    ; => 1
(find-if #'(lambda (x) (>= x 3))
         (vector 1 2 3 1 2 3))        ; => 3

(position-if-not #'evenp (list 1 2 3 1 2 3)) ; => 0
(position-if #'evenp (list 1 2 3 1 2 3))     ; => 1

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
(remove-duplicates (list 1 2 1 2 3 1 2 3 4)) ; => (1 2 3 4)

;;; -----------------------------------
;;; Other useful sequence manipulations
;;; -----------------------------------

(copy-seq (list 1 2 3)) ; => (1 2 3)
(reverse (list 1 2 3))  ; => (3 2 1)

(concatenate 'list (list 1 2 3) (list 4 5 6))        ; => (1 2 3 4 5 6)
(concatenate 'list (vector 1 2 3) (list 4 5 6))      ; => (1 2 3 4 5 6)

(concatenate 'vector (vector 1 2 3) (list 4 5 6))    ; => #(1 2 3 4 5 6)
(concatenate 'vector (vector 1 2 3) (vector 4 5 6))  ; => #(1 2 3 4 5 6)

(concatenate 'string "abc" "DEF")              ; => "abcDEF"
(concatenate 'string "abc" (list #\D #\E #\F)) ; => "abcDEF"

;;; -------------------------------------------------------
;;; Sorting functions (be careful, as they are destructive)
;;; -------------------------------------------------------

(defparameter my-seq-1 (list 2 1 5 3 4))
(defparameter my-seq-2 (list 2 1 5 3 4))

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
  xs)                       ; => (3 4)

;; In SBCL, we get the following warning:
;;   caught STYLE-WARNING:
;;     The return value of STABLE-SORT-LIST should not be discarded.

;; So to avoid this problem, we need to do the assignment:
(let ((xs (list 4 3 2 1)))
  (setf xs (sort xs #'<))
  xs)                       ; => (1 2 3 4)

;;; -------------------
;;; Merge two sequences
;;; -------------------

(merge 'vector (vector 1 3 5) (vector 2 4 6) #'<) ; => #(1 2 3 4 5 6)
(merge 'list (vector 1 3 5) (vector 2 4 6) #'<)   ; => (1 2 3 4 5 6)

;;; -----------------------
;;; Sub-sequence operations
;;; -----------------------

;; SUBSEQ
(subseq "newspaperman" 9)   ; => "man"
(subseq "newspaperman" 4 9) ; => "paper"
(subseq "newspaperman" 0 4) ; => "news"

(subseq (list 1 2 3 4) 3)   ; => (4)
(subseq (list 1 2 3 4) 1 3) ; => (2 3)

;; POSITION
(position 'man (list 'news 'paper 'man)) ; => 2
(position 'men (list 'news 'paper 'man)) ; => NIL

(position #\w "newspaperman") ; => 2
(position #\z "newspaperman") ; => NIL

(position 9 (vector 5 9 1 2)) ; => 1
(position 8 (vector 5 9 1 2)) ; => NIL

;;SEARCH
(search (list 'man) (list 'news 'paper 'man)) ; => 2
(search (list 'men) (list 'news 'paper 'man)) ; => NIL

(search "man" "newspaperman") ; => 9
(search "men" "newspaperman") ; => NIL

(search (vector 9) (vector 5 9 1 2)) ; => 1
(search (vector 8) (vector 5 9 1 2)) ; => NIL

;; MISMATCH
(mismatch "newspaperman" "newton")               ; => 3
(mismatch "newspaperman" "paperman")             ; => 0
(mismatch "newspaperman" "paperman" :from-end t) ; => 4

;; MEMBER (only works for lists)
(member 9 (list 5 9 1 2)) ; => (9 1 2) 
(member 8 (list 5 9 1 2)) ; => NIL

;;; -------------------
;;; Sequence predicates
;;; -------------------

;; One sequence only
(every #'evenp (list 1 2 3 4))    ; => NIL
(some #'evenp (list 1 2 3 4))     ; => T
(notany #'evenp (list 1 2 3 4))   ; => NIL
(notevery #'evenp (list 1 2 3 4)) ; => T

;; Two sequences
(every #'= (list 1 2 3 4) (list 5 4 3 2))    ; => NIL
(some #'= (list 1 2 3 4) (list 5 4 3 2))     ; => T
(notany #'= (list 1 2 3 4) (list 5 4 3 2))   ; => NIL
(notevery #'= (list 1 2 3 4) (list 5 4 3 2)) ; => T

;;; ------------------------------------------
;;; Sequence mapping functions: MAP and REDUCE
;;; ------------------------------------------

;; MAP function
(map 'list #'(lambda (x) (* x x)) (list 1 2 3)) ; => (1 4 9)
(map 'vector #'* (list 1 2 3) (list 4 5 6))     ; => #(4 10 18)

;; MAP-INTO function
(defparameter a (vector 1 0 0))
(defparameter b (vector 0 1 0))
(defparameter c (vector 0 0 1))

(map-into a #'+ a b c) ; => #(1 1 1)
(print a)              ; => #(1 1 1)

(map-into d #'+ a b c) ; => ERROR: The variable D is unbound.

(defparameter d (vector 4))
(defparameter e (vector 1 0 0 4))
(defparameter f (vector))

(map-into a #'+ a d) ; => #(5 1 1)
(print a)            ; => #(5 1 1)

(map-into a #'+ a e) ; => #(6 1 1)
(print a)            ; => #(6 1 1)

(map-into f #'+ a e) ; => #()
(print f)            ; => #()

;; REDUCE function
(reduce #'+ (vector 1 2 3 4))   ; => 10
(reduce #'min (vector 1 2 3 4)) ; => 1
(reduce #'max (vector 1 2 3 4)) ; => 4

(defun average (list)
  (/ (reduce #'+ list)
     (length list)))

(average (vector 1 2 3 4))      ; => 5/2

(reduce (lambda (x y) (+ (* x 10) y)) (list 1 2 3 4)) ; => 1234

(reduce #'append (list (list 1) (list 2))
        :initial-value (list 'i 'n 'i 't)) ; =>  (I N I T 1 2)
(reduce #'list (list 1 2 3 4)
        :initial-value 'foo)               ; => ((((FOO 1) 2) 3) 4)


;;;; ----------- ;;;;
;;;; HASH TABLES ;;;;
;;;; ----------- ;;;;

;;; -------------------------------------------
;;; Defining a hash table, and inserting values
;;; -------------------------------------------

(defparameter *fruits* (make-hash-table))
(gethash 1 *fruits*) ; => NIL, NIL
(setf (gethash 1 *fruits*) "orange")
(gethash 1 *fruits*) ; => "orange", T
(setf (gethash 2 *fruits*) "watermelon")
(gethash 2 *fruits*) ; => "watermelon", T

;; But if we want to use strings as keys, we need to define the hash table in
;; a slightly different way (using the ":test #'equal" parameter)

;; Incorrect:
(defparameter *stock* (make-hash-table))
(setf (gethash "orange" *stock*) 33)
(gethash "orange" *stock*) ; => NIL, NIL

;; Correct:
(defparameter *stock* (make-hash-table :test #'equal))
(setf (gethash "orange" *stock*) 33)
(gethash "orange" *stock*) ; => 33, T
(setf (gethash "watermelon" *stock*) 12)
(gethash "watermelon" *stock*) ; => 12, T

;;; --------------------------------------------------------------------
;;; Using MULTIPLE-VALUE-BIND to capture both values returned by GETHASH
;;; --------------------------------------------------------------------

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format nil "Value ~a actually present." value)
        (format nil "Value ~a because key not found." value))))

(setf (gethash 3 *fruits*) nil)
(gethash 3 *fruits*) ; => NIL, T

(show-value 1 *fruits*) ; => "Value ORANGE actually present."
(show-value 3 *fruits*) ; => "Value NIL actually present."
(show-value 4 *fruits*) ; => "Value NIL because key not found."

;;; -----------------------------------------------------------------
;;; Deleting a key/value pair, and completely clearing the hash table
;;; -----------------------------------------------------------------

(remhash 3 *fruits*)
(gethash 3 *fruits*) ; => NIL, NIL
(gethash 1 *fruits*) ; => "orange", T

(clrhash *fruits*)
(gethash 1 *fruits*) ; => NIL, NIL

;;; ----------------------------------------------------
;;; Iterating through a hash table with MAPHASH and LOOP
;;; ----------------------------------------------------

(defparameter *t* (make-hash-table))
(setf (gethash 1 *t*) "one")
(setf (gethash 2 *t*) "two")
(setf (gethash 3 *t*) "three")
(setf (gethash 4 *t*) "four")
(setf (gethash 5 *t*) "five")

(maphash #'(lambda (k v)
             (format t "~a => ~a~%" k v))
         *t*)

(loop for k being the hash-keys in *t* using (hash-value v)
   do (format t "~a => ~a~%" k v))

;; Adding or removing elements from a hash table while iterating over it are not
;; specified (undefined behaviour), with two exceptions:
;;   - you can use SETF with GETHASH to change the value of the current entry.
;;   - you can use REMHASH to remove the current entry.

;; Change the current value:
(maphash #'(lambda (k v)
             (if (oddp k)
                 (setf (gethash k *t*) (string-upcase v))))
         *t*)

(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *t*)

;; Remove the current value if its key is an even number:
(maphash #'(lambda (k v)
             (if (evenp k)
                 (remhash k *t*)))
         *t*)

(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *t*)
