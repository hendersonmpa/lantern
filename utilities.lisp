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
    (type-error () s)
    (parse-number:invalid-number () s)))

(defun handler-parse-universal (u)
  "universal to timestamp"
  (handler-case (local-time:universal-to-timestamp u)
    (type-error () nil)))


(defmacro once-only ((&rest names) &body body)
  "PCL Chapter 8"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@ (loop for g  in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@ (loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))


(defun nshuffle-vector (vector)
  "Use the Fisher-Yates algorithm to shuffle vector in place
PCL Chapter 23"
  (loop for idx downfrom (1- (length vector)) to 1
     for other = (random (1+ idx))
     do (unless (= idx other)
          (rotatef (aref vector idx) (aref vector other))))
  vector)


(defun flatten-once (list)
  "make a list of lists from a nested list of lists"
  (mapcan #'(lambda (x)
              (if (listp (car x)) (flatten-once x)
                  (list x)))
          list))

(defun flatten (list)
  "make a list from nested lists"
  (mapcan #'(lambda (x)
              (if (listp x) (flatten x)
                  (list x)))
          list))
