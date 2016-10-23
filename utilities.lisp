;;; file utilities.lisp
;;; Utility function for Lantern
(in-package :lantern)

(defun handler-parse-number (s)
  (handler-case (parse-number:parse-number s)
    (parse-error () s)
    (type-error () s)))
