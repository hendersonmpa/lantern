;;;; database.lisp
;;; Code from chapter 27 of PCL
(in-package :lantern)

;;; "database" goes here. Hacks and glory await!

;;; Table and Column classes
(defclass table ()
  ((rows :accessor rows  :initarg :rows :initform (make-rows))
   (schema :accessor schema :initarg :schema)))

(defparameter *default-table-size* 100)

(defun make-rows (&optional (size *default-table-size*))
  (make-array size :adjustable t :fill-pointer 0))

(defclass column ()
  ((name
    :reader name
    :initarg :name)
   (equality-predicate
    :reader equality-predicate
    :initarg :equality-predicate)
   (comparator
    :reader comparator
    :initarg :comparator)
   (default-value
       :reader default-value
     :initarg :default-value
     :initform nil)
   (value-normalizer
    :reader value-normalizer
    :initarg :value-normalizer
    :initform #'(lambda (v column) (declare (ignore column)) v))))


;;; Defining a schema
(defgeneric make-column (name type &optional default-value))

(defmethod make-column (name (type (eql 'string)) &optional default-value)
  (make-instance 'column
                 :name name
                 :comparator #'string<
                 :equality-predicate #'string=
                 :default-value default-value
                 :value-normalizer #'not-nullable))

(defmethod make-column (name (type (eql 'number)) &optional default-value)
  (make-instance 'column
                 :name name
                 :comparator #'<
                 :equality-predicate #'=
                 :default-value default-value))

(defun not-nullable (value column)
  (or value (error "Column ~a can't be null" (name column))))


(defclass interned-values-column (column)
  ((interned-values
    :reader interned-values
    :initform (make-hash-table :test #'equal))
   (equality-predicate :initform #'eql)
   (value-nornalizer :initform #'intern-for-column)))

(defun intern-for-column (value column)
  (let ((hash (interned-values column)))
    (or (gethash (not-nullable value column) hash)
        (setf (gethash value hash) value))))

(defmethod make-column (name (type (eql 'interned-string)) &optional default-value)
  (make-instance 'interned-values-column
                 :name name
                 :comparator #'string<
                 :default-value default-value))

(defun make-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))



(defparameter *ri* (make-instance 'table :schema *ri-schema*))

;;; Inserting values

(defun insert-row (names-and-values table)
  (vector-push-extend (normalize-row names-and-values (schema table)) (rows table)))

(defun normalize-row (names-and-values schema)
  (loop
     for column in schema
     for name = (name column)
     for value = (or (getf names-and-values name) (default-value column))
     collect name
     collect (normalize-for-column value column)))

(defun normalize-for-column (value column)
  (funcall (value-normalizer column) value column))

(defun load-database (file schema)
  "load a csv file into a database table"
  (let ((data-plist (load-csv file))
        (database (make-instance 'table :schema schema)))
    (dolist (row data-plist database)
      (insert-row row database))))


;;; Querying the database

(defun select (&key (columns t) from where distinct order-by)
  (let ((rows (rows from))
        (schema (schema from)))
    (when where
      (setf rows (restrict-rows rows where)))
    (unless (eql columns 't)
      (setf schema (extract-schema (mklist columns) schema))
      (setf rows (project-columns rows schema)))
    (when distinct
      (setf rows (distinct-rows rows schema)))
    (when order-by
      (setf rows (sorted-rows rows schema (mklist order-by))))
    (make-instance 'table :rows rows :schema schema)))

(defun mklist (thing)
  (if (listp thing) thing (list thing)))

(defun extract-schema (column-names schema)
  (loop for c in column-names collect (find-column c schema)))

(defun find-column (column-name schema)
  (or (find column-name schema :key #'name)
      (error "No column: ~a in schema: ~a" column-name schema)))

(defun restrict-rows (rows where)
  (remove-if-not where rows))

(defun project-columns (rows schema)
  (map 'vector (extractor schema) rows))

(defun distinct-rows (rows schema)
  (remove-duplicates rows :test (row-equality-tester schema)))

(defun sorted-rows (rows schema order-by)
  (sort (copy-seq rows) (row-comparator order-by schema)))

(defun extractor (schema)
  "returns a function that takes a row as an argument and returns a
new row containing only the columns specified in the schema it's
passed"
  (let ((names (mapcar #'name schema)))
    #'(lambda (row)
        (loop for c in names collect c collect (getf row c)))))

(defun row-equality-tester (schema)
  (let ((names (mapcar #'name schema))
        (tests (mapcar #'equality-predicate schema)))
    #'(lambda (a b)
        (loop for name in names and test in tests
           always (funcall test (getf a name) (getf b name))))))

(defun row-comparator (column-names schema)
  (let ((comparators (mapcar #'comparator (extract-schema column-names schema))))
    #'(lambda (a b)
        (loop
           for name in column-names
           for comparator in comparators
           for a-value = (getf a name)
           for b-value = (getf b name)
           when (funcall comparator a-value b-value) return t
           when (funcall comparator b-value a-value) return nil
           finally (return nil)))))

