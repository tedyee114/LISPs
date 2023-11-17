(
defun c:thwompo ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "PSELECT" "P" "")   
   (command "ISOLATEOBJECTS")
   (command "_layer" "_off" "R-PAVEMARK-LINE" "")
   (command "_layer" "_off" "GEOM-CHECKER-ARCHIVE" "")
)