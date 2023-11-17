(defun C:bdte ()
  (setq ANNA (ssget "X"))
  (command "PSELECT" DUPLICATELIST "")   

  ;(ssget "X" '((-4 . "<=") (0 . "LWPOLYLINE"))
)