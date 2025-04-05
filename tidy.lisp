;;; file tidy.lisp
(in-package :lantern)

;;; In tidy data
;;; 1.  Each variable forms a column.
;;; 2.  Each observation forms a row.
;;; 3.  Each type of observational unit forms a table.
;;; Manipulations: filter, transform, aggregate and sort


;;--------------------------------------------------
;;; SELECT
;;--------------------------------------------------

(defmacro get-xyz-data (data x y z)
  ;;; this is used to select data for plotting 
  `(let* ((accum nil)
	 (xyz-data (dolist (row ,data accum)
		     (destructuring-bind (&key ,x ,y ,z &allow-other-keys) row
		       (push (list ,x ,y ,z) accum)))))
    xyz-data))


;;--------------------------------------------------
;;; FILTER
;;--------------------------------------------------
;;; From http://lispblog.xach.com/post/147048601608/querying-plists


(defun compile-plist-query (query)
  ;;; From http://lispblog.xach.com/post/147048601608/querying-plists
  (labels ((callfun (object)
             (lambda (fun)
               (funcall fun object)))
           (compile-= (keyword value)
             (lambda (plist)
               (equal (getf plist keyword) value)))
           (compile-< (keyword value)
             (lambda (plist)
               (< (getf plist keyword) value)))
           (compile-> (keyword value)
             (lambda (plist)
               (> (getf plist keyword) value)))
           (compile-and (funs)
             (lambda (plist)
               (every (callfun plist) funs)))
           (compile-or (funs)
             (lambda (plist)
               (some (callfun plist) funs)))
           (compile-not (fun)
             (lambda (plist)
               (not (funcall fun plist)))))
    (let ((operator (first query))
          (operands (rest query)))
      (ecase operator
        (:=
         (compile-= (first operands) (second operands)))
        (:>
         (compile-> (first operands) (second operands)))
        (:<
         (compile-< (first operands) (second operands)))
        (:and
         (compile-and (mapcar #'compile-plist-query operands)))
        (:or
         (compile-or (mapcar #'compile-plist-query operands)))
        (:not
         (compile-not (compile-plist-query (first operands))))))))

(defun select-rows (query plists)
  (remove-if-not (compile-plist-query query) plists))


(defun get-cols (cols row)
  (labels ((get-cols-aux (cols accum)
             (if (null cols) accum
                 (let ((col (first cols)))
                   (get-cols-aux (rest cols)
                                 (cons (list col (getf row col)) accum))))))
    (get-cols-aux cols nil)))


(defun select-cols (cols plists)
  "Return a list containing the data in COLS for each row"
  (let ((accum nil))
    (dolist (row plists accum)
      (push (apply #'nconc (reverse (get-cols cols row))) accum))))


(defun select-columns (data &rest cols)
  "Return a list containing lists for each COL in DATA"
  (loop for col in cols
     collecting (mapcar #'(lambda (row)
                            (getf row col)) data)))

(defun select-points (row &rest cols)
  "Return a list containing lists for each COL in ROW"
  (loop for col in cols
     collecting (getf row col)))

(defun get-colnames (plist)
  "accept a plist and return the column names"
  (loop for entry in plist
     if (keywordp entry)
       collect entry))


(defmacro filter-data (data tests)
  "Filter rows from p-list data by key and test ((:KEY (TEST KEY VAL))*) "
  (alexandria:with-gensyms (row)
    (let* ((key-list (mapcar #'first tests))
           (test-list (mapcar #'second tests))
           (arg-list (mapcar
                      (lambda (s)
                        (find-symbol (symbol-name s))) key-list)))  ;; convert keys to symbols
      `(flet ((filter-p (,row)
                (funcall (lambda ,arg-list
                           (and ,@(mapcar (lambda (arg) `(not (null ,arg))) arg-list)
                                ,@test-list))
                         ,@(mapcar (lambda (key) `(getf ,row ,key)) key-list))))
         (remove-if-not #'filter-p ,data)))))
;;--------------------------------------------------
;;; AGGREGATE
;;--------------------------------------------------
;;; Generic query processing functions
(defun group-by-one (data col &key (tests (list #'equalp)))
  "Group on COL using TESTS"
  (group-by:group-by-repeated data
                              :keys (list #'(lambda (row) (getf row col)))
                              :tests tests))


;;; TODO generalize this, likely as a macro
(defun group-by-two (data &rest cols)
  "Group on COLS"
  (group-by:group-by-repeated data
                              :keys (list #'(lambda (row) (getf row (first cols)))
                                          #'(lambda (row) (getf row (second cols))))
                              :tests (list #'equalp)))


;; (create-groups (data-set-data *data-set*) :col :mrn :tests (list #'string-equal))


;;--------------------------------------------------
;;; TRANSFORM
;;--------------------------------------------------
(defun normalize-group (data &key group-col (denominator-col :mrn))
  "number of tests per unique mrn"
  (let ((grouped-data (group-by-one data group-col)))
    (mapcar (lambda (group)
              (list (first group)
                    (/ (length (rest group))
                       (length (remove-duplicates
                                (select-columns (rest group) :col denominator-col)
                                :test #'equalp))))) grouped-data)))


(defun collapse-group (group-data)
  "collapse a sparse group, return a single list"
  (let* ((data (rest group-data))
         (colnames (get-colnames (first data)))
         (index (first data)))
    (loop for list in (rest data) ;; loop over the rest of the groups
       do (loop for name in colnames ;; loop over each entry
             for entry = (getf list name)
             if entry
             do (setf (getf index name) entry)))
    index))

(defun column-freq (data &key (col :mrn))
  "Create a frequency hashtable at column :COL in data"
  (let ((freq (make-hash-table :test 'equalp)))
    (loop for row in data
	  do (incf (the fixnum (gethash (getf row col) freq 0))))
    (let ((freqlist nil))
      (maphash #'(lambda (k v) (push (cons v k) freqlist)) freq)
      (loop for (k . v) in (sort freqlist  #'> :key #'car)
	    collect (list v k)))))

;;--------------------------------------------------
;;; SORT
;;--------------------------------------------------
(defun top-n (data n)
  "Find the top n most frequent entries in out-put from column-freq"
  (let* ((sorted (sort data #'> :key #'second))
         (n-data (if (>= (length sorted) n)
                     (subseq sorted 0 n)
                     sorted))
         (accum nil))
    (dolist (row n-data (nreverse accum))
      (if (null (car row))
          (push (cons "empty" (rest row)) accum)
          (push row accum)))))
