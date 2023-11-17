(defun C:bdtf ()
  (setq ANNA (ssget "X"))
  (command "PSELECT" ANNA "")   

  ;(ssget "X" '((-4 . "<=") (0 . "LWPOLYLINE"))
)