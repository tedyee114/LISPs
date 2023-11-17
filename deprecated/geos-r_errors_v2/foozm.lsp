(defun C:foozm ()
  (setq LAYLIST (list "B-OVERHANG" "R-ROAD-ASPH"))
  (command "_layer" "m" "GEOS-R-TEMP" "") 
  (setq NLAYS (length LAYLIST))
  (setq i -1)
  (repeat NLAYS 
   (setq LAYCUR (LAYLIST i))                          ;"LAYCUR"=i'th element in "LAYLIST"
   (setq expert (getvar "expert"))
   (setvar "expert" 1)
   (command "._layer" "_off" "*" "_on" LAYCUR "")
   (setvar "expert" expert)
   (princ)
   (command "_mapclean" "GEOS-R-TEMP")
   (setq i (1+ i))
   ;(if
    ;count the number of objects on layer 0
   ;)
  )
  
  (command "_layer" "_on" "*")

)
