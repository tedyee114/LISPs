(defun C:foozh ()
  (setq L1 (getvar L1))
    (setq L1 (list 55 66 77)) 
  (setq NLAYS (sslength L1))
  (princ NLAYS)
)