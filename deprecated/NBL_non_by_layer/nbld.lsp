(defun C:nbld ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "/=") (62 . "nil")))
   (command "PSELECT" "P" "")   
   (command "ISOLATEOBJECTS")

  ;(setq color (cdr (assoc 62 name)))        ;attribute 62 is the color, if=nil, it's ByLayer
  ;(cdr (assoc 62 name))
)  