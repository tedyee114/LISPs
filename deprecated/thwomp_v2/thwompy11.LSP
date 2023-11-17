(
defun c:thwompm ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "select" "p" "")
)