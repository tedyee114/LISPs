(defun C:fooc ()
  (setq LAYCUR (getvar "LAYCUR"))
  (setvar "LAYCUR" "B-OVERHANG")  (cleaner (LAYCUR))
  (setvar "LAYCUR" "R-ROAD-ASPH") (cleaner (LAYCUR))
  (print "success!!")
  
  
)
