(defun C:foozj ()
  (setq L1 (getvar "L1"))
  (setq L1 (list "boverhang" 66 77)) 
  (setq NLAYS (length L1))
  (princ NLAYS)
)