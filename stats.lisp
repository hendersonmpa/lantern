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
