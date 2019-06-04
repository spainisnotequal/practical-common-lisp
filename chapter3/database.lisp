                                        ; ------------------- ;
                                        ; CD database example ;
                                        ; ------------------- ;

;; CDs and records

(defun make-cd (title artist rating ripped)
  (list :title title
        :artist artist
        :rating rating
        :ripped ripped))

;; Filing CDs

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

;; Looking at the database contents

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun dump-db () ;shorter version
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; Improving the user interaction

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
