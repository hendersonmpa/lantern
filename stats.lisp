(in-package :lantern)

;;; Histogram Bin Width
(defun fd-bins (data)
  "Determine the bin-width based on the data set
The Freedman-Diaconis rule is very robust and works well in
practice. The bin-width is set to h=2∗IQR/n^1/3. So the number of
bins is (max-min)/h."
  (let* ((max (apply #'max data))
         (min (apply #'min data))
         (n (length data))
         (iqr (max (iqr data) 1))       ;to prevent division by zero
         (h (* 2 (/ iqr (expt n (/ 1 3)))))
                                        ;(n-bins (ceiling (/ (- max min) h )))
         )
    h))



;; copied from RI project
;; works with the ts-scatterplot
;; TODO generalize 
(defun classify (ri-row sex age result)
  "Returns ROW with 1,2 or 3 appended if RI-ROW is applicable and RESULT is in REGION 1, 2 or 3"
  (destructuring-bind (&key ri-sex start end lower upper) ri-row
    (if (and (equalp sex ri-sex)
             (numberp age)
             (<= start age)
             (> end age)
             (numberp result))
        (cond ((< upper result) (list 3 sex age result))
              ((> lower result) (list 1  sex age result))
              (t (list 2 sex age result))))))

(defun classify-result (ds-row ri)
  "Returns T or NIL if RESULT is in REGION"
  (destructuring-bind (&key sex age result) ds-row
    (mapcan
     (lambda (ri-row)
       (classify ri-row sex age result)) ri)))


(defun classify-results (data ri)
  "Returns DATA in REGION of RI"
  (mapcar
   (lambda (ds-row)
     (classify-result ds-row ri)) data))
