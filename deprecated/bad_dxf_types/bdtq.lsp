(defun C:bdtq ()
  (setq ANNA (ssget "X"))                            ;"ANNA:"=everything
  (setq BOB  (ssget "X" '((0 . "LWPOLYLINE"))))      ;"BOB"= all polylines
  (setq CAM  (ssget "X" '((0 . "CIRCLES"))))         ;"CAM"= all circles
  (command "_SELECT" ANNA "R" CAM "")

  ;(command "PSELECT" ANNA "")   
  (command "ISOLATEOBJECTS")
)