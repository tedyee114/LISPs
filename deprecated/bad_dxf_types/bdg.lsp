(defun C:bdtg ()
  (setq BOB (ssget "X" '((0 . "LWPOLYLINE")(-4."/=")(8."U-MANHOLE"))))     ;"BOB"= all polylines
  ;(setq CAM (ssget "X" '(0 . "CIRCLES")))        ;"CAM"= all circles
  ;(setq ANNA (ssget "X"))

  (command "PSELECT" BOB "")   
  (command "ISOLATEOBJECTS")
)