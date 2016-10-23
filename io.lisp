;;; file io.lisp
;;; Functions for formatted input and output
(in-package :lantern)

(defun get-header (file-name)
  "Accepts a csv file and outputs a list of keywords"
  (with-open-file (in file-name
                      :direction :input)
    (let* ((row (cl-csv:read-csv-row in))
           (up-row (mapcar #'string-upcase row)))
      (mapcar #'alexandria:make-keyword up-row))))


(defparameter *header* (get-header "cars.csv"))

(defun load-csv (file-name)
  "Read a csv into a plist with column names as keys"
  ;; TODO: allow optional header values
  (flet ((process-row (row header)
           (let ((processed
                  (mapcar #'handler-parse-number row)))
             (mapcan #'list header processed))))
    (let ((header (get-header file-name)))
      (with-open-file (in file-name
                          :direction :input)
        (cl-csv:read-csv in
                         :map-fn #'(lambda (row) (process-row row header))
                         :skip-first-p 1
                         :separator #\,
                         :quote nil ;; there are quotes in comment strings
                         :unquoted-empty-string-is-nil t)))))
