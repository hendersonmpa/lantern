;;;; package.lisp

(defpackage #:lantern
  (:use #:cl
        ;;#:cl-arrows
	)
  (:export
   :load-csv
   :handler-parse-number
   :group-by-one
   :group-by-two
   :collapse-group
   :get-colnames
   :flatten
   :flatten-once
   :format-date
   :mean
   :sd
   :zscore))
