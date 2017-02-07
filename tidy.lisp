;;; file tidy.lisp
;;; In tidy data
;;; 1.  Each variable forms a column.
;;; 2.  Each observation forms a row.
;;; 3.  Each type of observational unit forms a table.
;;; Manipulations: filter, transform, aggregate and sort

;;--------------------------------------------------
;;; FILTER
;;--------------------------------------------------
;;; From http://lispblog.xach.com/post/147048601608/querying-plists
(defun compile-plist-query (query)
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
n        (:=
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

(defun select-columns (data &rest cols)
  "Return a list containing lists for each COL in DATA"
  (loop for col in cols
     collecting (mapcar #'(lambda (row)
                            (getf row col)) data)))


;;--------------------------------------------------
;;; AGGREGATE
;;--------------------------------------------------
;;; Generic query processing functions
(defun create-groups (data &key col (tests (list #'equalp)))
  "Group on COL using TESTS"
  (group-by:group-by-repeated data
                              :keys (list #'(lambda (row) (getf row col)))
                              :tests tests))
;; (create-groups (data-set-data *data-set*) :col :mrn :tests (list #'string-equal))


;;--------------------------------------------------
;;; TRANSFORM
;;--------------------------------------------------
(defun normalize-group (data &key group-col (denominator-col :mrn))
  "number of tests per unique mrn"
  (let ((grouped-data (create-groups data :col group-col)))
    (mapcar (lambda (group)
              (list (first group)
                    (/ (length (rest group))
                       (length (remove-duplicates
                                (collect-column (rest group) :col denominator-col)
                                :test #'equalp))))) grouped-data)))

 (defun column-freq (data &key (col :mrn))
  "Create a frequency hashtable at column :COL in data"
  (let ((freq (make-hash-table :test 'equalp))))
  (loop for row in data
     do (incf (the fixnum (gethash (getf row col) freq 0))))
  (let ((freqlist nil))
    (maphash #'(lambda (k v) (push (cons v k) freqlist)) freq)
    (loop for (k . v) in (sort freqlist  #'> :key #'car)
       collect (list v k))))

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
