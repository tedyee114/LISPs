(defun C:foor()
  ;(setq LAYLIST '(B-OVERHANG R-ROAD-ASPH))
  (command "_layer" "m" "GEOS-R-TEMP" "") 
  (setq LAYCUR "B-OVERHANG")
  ;; (setq LAYS (sslength LAYLIST))
  ;; (setq i -1)
  ;; (repeat LAYS 
  ; (setq LAYCUR (LAYLIST i))                          ;"LAYCUR"=i'th element in "LAYLIST"
   (setq expert (getvar "expert"))
   (setvar "expert" 1)
   (command "._layer" "_off" "*" "_on" LAYCUR "")
   (setvar "expert" expert)
   (princ)
   (command "_mapclean" "GEOS-R-TEMP")
   ;(setq (1+ i))
   ;(if
    ;count the number of objects on layer 0
   ;)
  
  
  (command "_layer" "_on" "*")

)
