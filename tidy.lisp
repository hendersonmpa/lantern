;;; file tidy.lisp
;;; In tidy data
;;; 1.  Each variable forms a column.
;;; 2.  Each observation forms a row.
;;; 3.  Each type of observational unit forms a table.
;;; Manipulations: filter, transform, aggregate and sort


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

;; (select-rows '(:and (:> :mpg 15) (:= :cyl 4)
;;                 (:not (:= :last-name "Beane")))
;;               *people*)
