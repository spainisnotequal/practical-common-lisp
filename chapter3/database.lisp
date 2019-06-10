                                        ; ------------------- ;
                                        ; CD database example ;
                                        ; ------------------- ;

;; ---------------
;; CDs and records
;; ---------------

(defun make-cd (title artist rating ripped)
  (list :title title
        :artist artist
        :rating rating
        :ripped ripped))

;; ----------
;; Filing CDs
;; ----------

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 nil))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

;; --------------------------------
;; Looking at the database contents
;; --------------------------------

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun dump-db () ; shorter version
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; ------------------------------
;; Improving the user interaction
;; ------------------------------

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*) ; don't wait for a new line
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; -------------------------------
;; Saving and loading the database
;; -------------------------------

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; ---------------------
;; Querying the database
;; ---------------------

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10)) ; extract the even elements
(remove-if-not #'oddp '(1 2 3 4 5 6 7 8 9 10)) ; extract the even elements

;; Select the even elements without using evenp
(remove-if-not
 #'(lambda (x) (= 0 (mod x 2)))
 '(1 2 3 4 5 6 7 8 9 10))

;; Select the odd elements without using oddp
(remove-if-not
 #'(lambda (x) (= 1 (mod x 2)))
 '(1 2 3 4 5 6 7 8 9 10))

;; Select all the Dixie Chicks' albums
(remove-if-not
 #'(lambda (cd)
     (equal (getf cd :artist) "Dixie Chicks"))
 *db*)

;; Select a CD using the artist name
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd)
       (equal (getf cd :artist) artist))
   *db*))

(select-by-artist "Dixie Chicks")

;; Select a CD using any field (title, artist, rating, or ripped)
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; Define an artist selection function
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; Use select and artist-seletor to select CDs
(select (artist-selector "Dixie Chicks"))
(select (artist-selector "Kathy Mattea"))

;; Define a general selector-function generator
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title
           (equal (getf cd :title) title)
           t)
       (if artist
           (equal (getf cd :artist) artist)
           t)
       (if rating
           (equal (getf cd :rating) rating)
           t)
       (if ripped-p
           (equal (getf cd :ripped) ripped)
           t))))

(select (where :artist "Dixie Chicks"))
(select (where :rating 8 :ripped nil))

;; -------------------------
;; Updating existing records
;; -------------------------

;; Update CD function
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(update (where :artist "Dixie Chicks") :rating 11)

;; Delete CD function
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(delete-rows (where :rating 7))

;; ------------------------------------
;; Removing duplication and winning big
;; ------------------------------------

(defun make-comparison-expr (field value)    ; wrong
  (list equal (list getf cd field) value))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

(defun make-comparison-expr (field value)    ; simpler version
  `(equal (getf cd ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

(macroexpand-1 '(where :rating 10 :title "Give us a break"))

(select (where :rating 11 :title "Home"))
