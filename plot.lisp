;;; file plot.lisp
;;; Functions to plot data from a lists of p-lists (lopl)
(in-package :lantern)


;; (defun ts-scatterplot (xyz-data &key (title "scatterplot") (xlabel nil) (ylabel nil))
;;   (let ((file-name (merge-pathnames *figures-dir*
;;                                      (concatenate 'string title ".pdf")))
;; 	 ;(xyz-data (get-xyz-data data-set x y z)) ;; not sure why this doesn't work 
;; 	)
;;     (eazy-gnuplot:with-plots (*standard-output* :debug t)
;;       (format t "load ~s ~%" "/home/mpah/lisp/site/lantern/default.plt" )
;;       (format t "unset colorbox ~%")
;;       (eazy-gnuplot:gp-setup :output file-name
;;                              :terminal '(pdfcairo enhanced font "Verdana,8")
;;                                         ;'(svg :size (810 500) enhanced font "Verdana,8")
;;                              :key '(off)
;; 			     :title title
;; 			     :xlabel xlabel
;;                              :ylabel ylabel
;;                              :ytic '(nomirror font ",8")
;;                              :xtic '(nomirror font ",8")
;;                              :tic '(nomirror out scale 0.75)
;;                              :grid '(ytics lc rgb ("'#bbbbbb'") lw 1 lt 0)
;;                              ;; :grid '(xtics lc rgb ("'#bbbbbb'") lw 1 lt 0)
;;                              :border '(3 front linecolor rgb ("'#808080'") lt 1)
;;                              :pointsize .5
;;                              :style '(line 1 linetype 1 lc rgb ("'#71637D'") lw 1)
;;                              :style '(line 2 linetype 3 lc rgb ("'#3288BD'") lw 1 pointtype 4))
;;       (eazy-gnuplot:plot
;;        (lambda ()
;;          (dolist (row xyz-data)
;;            (format t "~&~{~d ~d ~d~%~}" row)))
;;        :using '("1:2:3 with points palette")) file-name)))




;;;; run an example
;; (defparameter *car-data*  (load-csv #P"/home/mpah/lisp/site/lantern/cars.csv"))
;; (defparameter *figures-dir* "~/lisp/site/lantern/figures/")


;; (let ((xyz-data (get-xyz-data *car-data* mpg disp cyl)))
;;    (ts-scatterplot xyz-data :xlabel "mpg" :ylabel "disp"))

;; (let ((xyz-data (get-xyz-data *tsh-data* age result result)))
;;   (ts-scatterplot xyz-data :title "tsh" :xlabel "age" :ylabel "tsh"))


