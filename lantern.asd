;;;; lantern.asd

(asdf:defsystem #:lantern
  :description "Shed a bit of light on your data"
  :author "Your Name <your.name@example.com>"
  :license "LLGPL"
  :depends-on (#:cl-csv
               #:eazy-gnuplot
               #:alexandria
               #:parse-number
               #:cl-arrows)
  :serial t
  :components ((:file "package")
               (:file "lantern")
               (:file "utilities")
               (:file "stats")
               (:file "io")
               (:file "tidy")))
