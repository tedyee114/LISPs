(
defun c:thwompp ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "ISOLATEOBJECTS")
   (command "SELECT" "p" "")
)