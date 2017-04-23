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


(defmacro once-only ((&rest names) &body body)
  "Chapter 8 PCL"
  (let ((gensyms (loop for n in names collect (gensyms))))
    `(let (,@(loop for g in gensyms collect `(,g (gensyms))))
       `(let (,,@ (loop for g  in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@ (loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))
