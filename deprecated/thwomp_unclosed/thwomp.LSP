(
defun c:thwompr ()
   (command "_layer" "_off" "R-PAVEMARK-LINE" "")
   (command "_layer" "_off" "GEOM-CHECKER-ARCHIVE" "")
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "Pselect" "P" "")   
   (command "ISOLATEOBJECTS")
)