(defun C:bdto ()
  (setq BOB (ssget "X" '((0 . "LWPOLYLINE"))))       ;"BOB"= all polylines
  (command "_LAYLCK" BOB "")                        ;locks all polylines
  ;(setq CAM (ssget "X" '(0 . "CIRCLES")))          ;"CAM"= all circles
  
  (setq DAN (ssget ":L"))                           ;"DAN"= all unlocked layers
  (command "_HIDEOBJECTS" DAN "")                   ;hides all unlocked layers
  (command "_LAYULK" "ALL" "")                      ;unlocks all layers
  ;(command "PSELECT" ANNA "")   
  ;(command "ISOLATEOBJECTS")
)