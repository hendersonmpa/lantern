(in-package :lantern)

;;;;; Macros

;; This macro makes assertions more readable.  There are several special
;; types defined: :probability (:prob), :positive-integer (:posint),
;; :positive-number (:posnum), :number-sequence (:numseq),
;; :positive-integer-sequence (:posintseq), :probability-sequence
;; (:probseq), :nonzero-number-sequence (:nonzero-numseq) and :percentage
;; (:percent).  Other assertions are assumed to be internal types.  The
;; arguments to test-variables are lists.  The first element of the list is
;; a variable name, and the second element is either a special or built-in
;; type.  If the variable binding is not of the type specified, and error is
;; signalled indicating the problem.  One variable may have multiple type
;; requirements, which are conjunctive.

(defmacro test-variables (&rest args)
  (let ((assertions nil))
    (dolist (arg args (append `(or ,@(nreverse assertions))))
      (let* ((name (first arg))
             (type (second arg))
             (test (case type
                     ((:probability :prob)
                      `(and (numberp ,name) (not (minusp ,name)) (<= ,name 1)))
		     ((:number :num)
                      `(numberp ,name))
                     ((:positive-integer :posint)
                      `(and (integerp ,name) (plusp ,name)))
                     ((:positive-number :posnum)
                      `(and (numberp ,name) (plusp ,name)))
                     ((:number-sequence :numseq)
                      `(and (typep ,name 'sequence) (every #'numberp ,name)
                            (not (null ,name))))
                     ((:nonzero-number-sequence :nonzero-numseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                            (every (lambda (x) (and (numberp x) (not (= 0 x))))
                                   ,name)))
                     ((:probability-sequence :probseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                            (every (lambda (x) (and (numberp x) (not (minusp x))
                                                    (<= x 1.0))) ,name)))
                     ((:positive-integer-sequence :posintseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                            (every (lambda (x) (and (typep x 'integer) (plusp
                                                                        x)))
                                   ,name)))
                     (:percentage
                      `(and (numberp ,name) (plusp ,name) (<= ,name 100)))
                     (:test (third arg))
                     (t `(typep ,name ',type))))
             (message `(error
                        ,(if (eql type :test)
                             "~a"
                             (format nil "~a = ~~a is not a ~a" name
                                     (case type
					 
				       ((:number :num)
                                        "number")
                                       ((:positive-integer :posint)
                                        "positive integer")
                                       ((:positive-number :posnum)
                                        "positive number")
                                       ((:probability :prob) "probability")
                                       ((:number-sequence :numseq)
                                        "sequence of numbers")
                                       ((:nonzero-number-sequence
                                         :nonzero-numseq)
                                        "sequence of non-zero numbers")
                                       ((:positive-integer-sequence :posintseq)
                                        "sequence of positive integers")
                                       ((:probability-sequence :probseq)
                                        "sequence of probabilities")
                                       ((:percent :percentile) "percent")
                                       (t type))))
                        ,name)))
        (push `(unless ,test ,message) assertions)))))


(defmacro square (x)
    `(* ,x ,x))

  (defmacro underflow-goes-to-zero (&body body)
    "Protects against floating point underflow errors and sets the value to 0.0 instead."
    `(handler-case
         (progn ,@body)
       (floating-point-underflow (condition)
         (declare (ignore condition))
         (values 0.0d0))))

;;; Summary statistics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Descriptive statistics
;;;  copied and then adapted from lhstats
;;;

;; Rosner 10
(defun mean (sequence)
  (test-variables (sequence :numseq))
  (/ (reduce #'+ sequence) (length sequence)))

;; Rosner 12 (and 19)
(defun median (sequence)
  (test-variables (sequence :numseq))
  (percentile sequence 50))

;; Rosner 14
;; Rob St. Amant <stamant@csc.ncsu.edu> suggested using a hash table
;; instead of an alist.
(defun mode (sequence)
  "Returns two values: a list of the modes and the number of times
they occur."
  (test-variables (sequence :numseq))
  (let ((count-table (make-hash-table))
        mode (mode-count 0))
    (map nil (lambda (elt) (incf (gethash elt count-table 0))) sequence)
    (maphash (lambda (key value)
               (when (> value mode-count)
                 (setf mode key
                       mode-count value)))
             count-table)
    (values mode mode-count)))


;; Rosner 16
(defun geometric-mean (sequence &optional (base 10))
  (test-variables (sequence :nonzero-numseq) (base :posnum))
  (expt base (mean (map 'list (lambda (x) (log x base)) sequence))))

;; Rosner 18
(defun range (sequence)
  (test-variables (sequence :numseq))
  (- (reduce #'max sequence) (reduce #'min sequence)))

;; Rosner 19
;; NB: Aref is 0 based!
(defun percentile (sequence percent)
  (test-variables (sequence :numseq) (percent :percentage))
  (let* ((sorted-vect (coerce (sort (copy-seq sequence) #'<) 'simple-vector))
         (n (length sorted-vect))
         (k (* n (/ percent 100)))
         (floor-k (floor k)))
    (if (= k floor-k)
        (/ (+ (aref sorted-vect k)
              (aref sorted-vect (1- k)))
           2)
        (aref sorted-vect floor-k))))

;; Rosner 21
(defun variance (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (/ (reduce #'+ (map 'list (lambda (x) (square (- mean x))) sequence))
       (1- n))))

;; Rosner 21
(defun standard-deviation (sequence)
  (test-variables (sequence :numseq))
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list (lambda (x) (square (- mean x))) sequence))
             (1- n)))))

(defun sd (sequence)
  (standard-deviation sequence))


;; Rosner 24
(defun coefficient-of-variation (sequence)
  (* 100 (/ (standard-deviation sequence) (mean sequence))))

;; Rosner 172
(defun standard-error-of-the-mean (sequence)
  (/ (standard-deviation sequence) (sqrt (length sequence))))

(defun standard-error (sequence)
  (standard-error-of-the-mean sequence))

;; mpah additions

(defun zscore (data-point sd mean)
  (test-variables (data-point :num) (sd :posnum) (mean :num))
  (/(- data-point mean) sd))



;;; Histogram Bin Width
;; (defun fd-bins (data)
;;   "Determine the bin-width based on the data set
;; The Freedman-Diaconis rule is very robust and works well in
;; practice. The bin-width is set to h=2∗IQR/n^1/3. So the number of
;; bins is (max-min)/h."
;;   (let* ((max (apply #'max data))
;;          (min (apply #'min data))
;;          (n (length data))
;;          (iqr (max (iqr data) 1))       ;to prevent division by zero
;;          (h (* 2 (/ iqr (expt n (/ 1 3)))))
;;                                         ;(n-bins (ceiling (/ (- max min) h )))
;;          )
;;     h))



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


