(defun C:fooza ()
  (setq LAYLIST list("B-OVERHANG" "R-ROAD-ASPH")) 
  (setq NLAYS (sslength LAYLIST))
  (princ NLAYS)
)