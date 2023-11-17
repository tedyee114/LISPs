(
defun c:thwompq ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "Pselect" "P" "")   
   (command "ISOLATEOBJECTS")
)