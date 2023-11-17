(defun C:fook ()
  (setq LAYCUR "B-OVERHANG")  (cleaner ("B-OVERHANG"))
  (setq LAYCUR "R-ROAD-ASPH") (cleaner ("R-ROAD-ASPH"))
  (print "success!!")
)

 (defun cleaner (LAYCUR)
    (command "_layer" "m" "GEOS-R-TEMP" "") 
    (setq expert (getvar "expert"))
    (setvar "expert" 1)
    (command "._layer" "_off" "*" "_on" LAYCUR "")
    (setvar "expert" expert)
    (princ (sslength (setq num (ssget "_A" '((8."GEOS-R-TEMP"))))))
    (command "_mapclean" "GEOS-R-TEMP")
    ;(if
     ;count the number of objects on layer 0
    ;)
    (command "_layer" "_on" "*")
 )
