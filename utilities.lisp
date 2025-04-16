;;; file utilities.lisp
;;; Utility function for Lantern
(in-package :lantern)

(defun prompt-read (prompt)
  "Create a prompt and read input"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun handler-parse-number (s)
  "Convert string to number"
  (handler-case (parse-number:parse-number s)
    (parse-error () s)
    (type-error () s)
    (parse-number:invalid-number () s)))


(defun handler-parse-integer (s)
  (handler-case (parse-integer s :junk-allowed t)
    (parse-error () 0)
    (type-error () 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Date and Time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handler-parse-universal (u)
  "universal to timestamp"
  (handler-case (local-time:universal-to-timestamp u)
    (type-error () nil)))


(defun format-datetime (datetime-list)
  "Convert ?m-?d-YYYY and ?h:?m:ss to ISO date-time format YYYY-MM-DD HH:MM:SS.SSS."
  (flet ((iso-date (date)
           (cl-ppcre:register-groups-bind
               ((#'parse-integer month) (#'parse-integer day) (#'parse-integer year))
               ("(\\d{1,2})/(\\d{1,2})/(\\d{4})" date)
             (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))
         (iso-time (time)
           (cl-ppcre:register-groups-bind
               ((#'parse-integer hours) (#'parse-integer minutes) (#'parse-integer seconds))
               ("(\\d{1,2}):(\\d{1,2}):(\\d{2})" time)
             (format nil "~2,'0d:~2,'0d:~2,'0d.000" hours minutes seconds))))
    (let ((date (iso-date (first datetime-list)))
          (time (iso-time (second datetime-list))))
      (concatenate 'string date " " time))))

(defun format-date (date-string)
  (let* ((univeral-time (cl-date-time-parser:parse-date-time date-string))
	 (time-stamp (local-time:universal-to-timestamp univeral-time))
	 ;; for some reason cl-date-time-parser:parse-date-time interprets "04-Feb-24" as "03-Feb-24"
	 (corrected-time-stamp (local-time:timestamp+ time-stamp 1 :day))
	 (new-date-string (local-time:format-timestring nil corrected-time-stamp
							:format '(:year "-" (:month 2) "-" (:day 2)))))
    new-date-string))



(defun minutes-float (time-string)
  "convert HH:MM:SS to a minutes float"
  (cl-ppcre:register-groups-bind
      ((#'parse-integer hours) (#'parse-integer minutes) (#'parse-integer seconds))
      ("(\\d+):(\\d{2}):(\\d{2})" time-string)
    (float (+ (* hours 60)
              minutes
              (/ seconds 60)))))


;; format a time string
(defun today-string ()
  (local-time:format-timestring nil (local-time:now) :format  '(:year "-" :month "-" :day)))
;; (day- (today-string) 1)

(defun yesterday-string ()
  (day- (today-string) 1))
;; (day- (today-string) 1)


(defun age-string-num (age-string)
  "TODO Convert an age string to a years float"
  (cl-ppcre:register-groups-bind
      ((#'parse-integer number) units)
      ("(\\d{1,3}) (\\w{5,6})" age-string)
    (float (cond ((equalp units "years") number)
                 ((equalp units "months")
                  (/ number 12))
                 (t 0)))))

(defun day- (date-string n)
  "accept a date-string and subtract n days"
  (let* ((est-offset (* -5 60 60 )) ;; seconds from GMT which is annoying and changes based on DST
	 ;; needs to be fix
	 (date-timestring (local-time:parse-timestring date-string :offset est-offset))
	  ;; used for day booked date
	 (date-timestamp (local-time:timestamp- date-timestring n :day))
	 (new-timestring (local-time:format-timestring  nil date-timestamp :format (list :year "-" :month "-" :day))))
    new-timestring))

(defun day+ (date-string n)
  "accept a date-string and subtract n days"
  (let* ((est-offset (* -5 60 60 )) ;; seconds from GMT which is annoying and changes based on DST
	 ;; needs to be fix
	 (date-timestring (local-time:parse-timestring date-string :offset est-offset))
	  ;; used for day booked date
	 (date-timestamp (local-time:timestamp+ date-timestring n :day))
	 (new-timestring (local-time:format-timestring  nil date-timestamp :format (list :year "-" :month "-" :day))))
      new-timestring))
  





(defmacro once-only ((&rest names) &body body)
  "PCL Chapter 8"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@ (loop for g  in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@ (loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))


(defun nshuffle-vector (vector)
  "Use the Fisher-Yates algorithm to shuffle vector in place
PCL Chapter 23"
  (loop for idx downfrom (1- (length vector)) to 1
     for other = (random (1+ idx))
     do (unless (= idx other)
          (rotatef (aref vector idx) (aref vector other))))
  vector)


(defun flatten-once (list)
  "make a list of lists from a nested list of lists"
  (mapcan #'(lambda (x)
              (if (listp (car x)) (flatten-once x)
                  (list x)))
          list))

(defun flatten (list)
  "make a list from nested lists"
  (mapcan #'(lambda (x)
              (if (listp x) (flatten x)
                  (list x)))
          list))
