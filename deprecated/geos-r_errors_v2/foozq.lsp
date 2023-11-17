(defun C:foozq ()
  ;(command "_layer" "m" "GEOS-R-TEMP" "") 
  (print "Dummy layer made.")
  (setq everything (ssget "X"))
  (setq total (sslength everything))
  (setq j -1)
  (setq LAYLIST (ssadd))
  (repeat total
    (ENTLAY (cdr (assoc 8 (entget CURRENT))))         ;"ENTLAY"= searches within the entity info for value type 8 (layer)
    (if (member ENTLAY (list ENTLIST))
      (ssadd ENTLAY LAYLIST)
    )

  )
    (setq NLAYS (length LAYLIST))
  (setq i -1)
  (princ i) (princ NLAYS)
  ;;  (repeat NLAYS 
  ;;   (setq LAYCUR (LAYLIST i))                          ;"LAYCUR"=i'th element in "LAYLIST"
  ;;   (setq expert (getvar "expert"))
  ;;   (setvar "expert" 1)
  ;;   (command "._layer" "_off" "*" "_on" LAYCUR "")
  ;;   (setvar "expert" expert)
  ;;   (princ)
  ;;   (command "_mapclean" "GEOS-R-TEMP")
  ;;   (setq i (1+ i))
  ;;   ;(if
  ;;    ;count the number of objects on layer 0
  ;;   ;)
  ;;  )
  
  ;; (command "_layer" "_on" "*")

)
