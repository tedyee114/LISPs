(
defun c:thwompo ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "SELECT" "p" "")
   (command "ISOLATEOBJECTS")
)