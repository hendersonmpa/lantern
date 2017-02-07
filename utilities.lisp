;;; file utilities.lisp
;;; Utility function for Lantern
(in-package :lantern)

(defun prompt-read (prompt)
  "Create a prompt and read input"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun handler-parse-number (s)
  "Convert string to number"
  (handler-case (parse-number:parse-number s)
    (parse-error () s)
    (type-error () s)))

(defun handler-parse-universal (u)
  "universal to timestamp"
  (handler-case (local-time:universal-to-timestamp u)
    (type-error () nil)))
