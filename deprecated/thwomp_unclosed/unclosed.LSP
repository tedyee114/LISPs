(
defun c:unclosed ()
   (command "_layer" "_off" "R-PAVEMARK-LINE" "")
   (command "_layer" "_off" "GEOM-CHECKER-ARCHIVE" "")
   (ssget "X" '((0 . "LWPOLYLINE") (70 . 1)))
   (command "Pselect" "P" "")   
   (command "ISOLATEOBJECTS")
)