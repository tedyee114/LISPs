(defun C:bdth ()
  (setq BOB (ssget "X" '((0 . "LWPOLYLINE"))))     ;"BOB"= all polylines
  ;(setq CAM (ssget "X" '(0 . "CIRCLES")))        ;"CAM"= all circles
  ;(setq ANNA (ssget "X"))

  (command "PSELECT" BOB "")   
  (command "ISOLATEOBJECTS")
)