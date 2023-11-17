(defun c:gerd ()
  (setq blocks (ssget "X" '((0 . "INSERT"))))                ;"blocks"=list of blocks
  (setq numblocks (sslength blocks))                                  ;"numblocks"=number of blocks
  (princ numblocks)
  (command "_mapclean" "gera" "M" "CLOSE")                               ;places a block at the intersections
  (setq newblocks (ssget "X" '((0 . "INSERT"))))      ;"newblocks"=list of blocks including the markers
  (setq newnum (sslength newblocks))    
  (princ newnum)
)