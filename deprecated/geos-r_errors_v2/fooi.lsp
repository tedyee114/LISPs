(defun C:fooi ()
  ;(setq LAYLIST)
  ;(Setq NUM)
  ;(setq LAYCUR (LAYLIST NUM))
  ;(command "_LAYFRZ" "*" "")
  ;(command "_LAYTHW" "B-OVERHANG" "")
 (setq clayer (getvar "clayer") expert (getvar "expert"))
 (setvar "expert" 1)
 (command "._layer" "_off" "*" "_on" clayer "")
 (setvar "expert" expert)
 (princ)
)
