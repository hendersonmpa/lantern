;;; file test.lisp
(in-package :lantern)

;;(defparameter *car-data* (load-csv "cars.csv"))

(defparameter *car-schema*
  (make-schema
   '((:name string)
     (:mpg number)
     (:cyl number)
     (:disp number)
     (:hp number)
     (:drat number)
     (:wt number)
     (:qsec number)
     (:vs number)
     (:am number)
     (:gear number)
     (:carb number))))


(defparameter *car-db* (load-database "cars.csv" *car-schema*))

(select :from *car-db* :where (matching *car-db* :name "Merc"))

(insert-row (first *car-data*) *car-db*)

(normalize-row (subseq *car-data* 0 10) *car-schema*)

(extract-schema '(:name :mpg) *car-schema*)
