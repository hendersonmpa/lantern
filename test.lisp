;;; file test.lisp
(in-package :lantern)


;;; testing for database.lisp
;;(defparameter *car-data* (load-csv "cars.csv"))

(defparameter *car-schema*
  (make-schema
   '((:model string)
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
(select :from *car-db* :where (matching *car-db* :model "Toyota Corrolla"))

(normalize-row (subseq *car-data* 0 10) *car-schema*)

(extract-schema '(:name :mpg) *car-schema*)

;;; all entries with the same number of cylinders as a Corrolla
(select
 :columns '(:model :cyl)
 :from *car-db*
 :where (in :cyl
            (select
             :columns :cyl
             :from *car-db*
             :where (matching *car-db* :model "Toyota Corolla"))))

(let ((table (sort-rows *car-db* :mpg)))
  (do-rows (row table)
    (with-column-values (model mpg cyl) row
      (format t "~a gets ~d mpg with ~d cylinders~%" model mpg cyl))))

(show (sort-rows *car-db* :mpg))

(row-comparator '(:mpg) (schema *car-db*))
