;;;; lantern.asd

(asdf:defsystem #:lantern
  :description "Describe lantern here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-csv
               #:eazy-gnuplot
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "lantern")))
