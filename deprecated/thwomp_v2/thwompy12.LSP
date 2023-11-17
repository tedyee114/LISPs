(
defun c:thwompn ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "select" "p" "")
   (command "isolate")
)