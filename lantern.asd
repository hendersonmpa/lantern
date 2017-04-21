;;;; lantern.asd

(asdf:defsystem #:lantern
  :description "Shed a bit of light on your data"
  :author "Matthew Henderson"
  :license "LLGPL"
  :depends-on (#:cl-csv
               #:group-by
               #:eazy-gnuplot
               #:alexandria
               #:parse-number
               #:cl-arrows
               #:local-time)
  :serial t
  :components ((:file "package")
               (:file "lantern")
               (:file "utilities")
               (:file "stats")
               (:file "io")
               (:file "tidy")
               (:file "database")))
