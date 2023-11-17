(defun C:bdtt ()
  (setq ANNA (ssget "X"))                            ;"ANNA:"=everything
  (setq BOB  (ssget "X" '((0 . "LWPOLYLINE"))))      ;"BOB"= all polylines
  (setq CAM  (ssget "X" '((0 . "CIRCLES"))))         ;"CAM"= all circles
  ;(command "PSELECT" ANNA "R" CAM "")

  ;(command "PSELECT" ANNA "")   
  (command "ISOLATEOBJECTS" ANNA "R" CAM "")
)