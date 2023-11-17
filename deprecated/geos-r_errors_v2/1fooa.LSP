(defun C:fooa ()
  (print "Dummy layer made.")
  
  (setq LAYCUR (getvar "LAYCUR"))
  (setvar "LAYCUR" "B-OVERHANG")  (cleaner)
  (setvar "LAYCUR" "R-ROAD-ASPH")  (cleaner)
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
