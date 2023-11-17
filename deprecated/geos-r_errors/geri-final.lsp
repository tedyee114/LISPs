(defun c:geri ()
  (command "_layer" "m" "GEOS-R ERRORS" "")
  (setq blocks (ssget "X" '((0 . "INSERT"))))                ;"blocks"=list of blocks
  (command "_mapclean" "gera")                               ;places a block at the intersections
  (setq newblocks (ssget "X" '((0 . "INSERT"))))             ;"newblocks"=list of blocks including the markers
  (if (sslength blocks)=(sslength newblocks)                 ;"detected" value reflects whether the number of blocks has changed
    (setq detected 0)
    (setq detected 1)
  )
)