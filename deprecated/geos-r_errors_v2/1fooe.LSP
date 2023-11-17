(defun C:fooe ()
  (setq LAYCUR (getvar "LAYCUR"))
  (setq "LAYCUR" "B-OVERHANG")  (cleaner (LAYCUR))
  (setq "LAYCUR" "R-ROAD-ASPH") (cleaner (LAYCUR))
  (print "success!!")
  
  
)
(defun cleaner (LAYCUR)
   (command "_layer" "m" "GEOS-R-TEMP" "") 
   (setq expert (getvar "expert"))
   (setvar "expert" 1)
   (command "._layer" "_off" "*" "_on" LAYCUR "")
   (setvar "expert" expert)
   (princ)
   (command "_mapclean" "GEOS-R-TEMP")
   ;(if
    ;count the number of objects on layer 0
   ;)
   (command "_layer" "_on" "*")
 )
