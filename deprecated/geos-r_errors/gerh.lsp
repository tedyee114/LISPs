(defun c:gerh ()
  (setq blocks (ssget "X" '((0 . "INSERT"))))                ;"blocks"=list of blocks
  (setq numblocks (sslength blocks))                                  ;"numblocks"=number of blocks
  (princ numblocks)
  (command "_mapclean" "gera")                               ;places a block at the intersections
  (action_tile "Mark All")
;  (setq newblocks (ssget "X" '((0 . "INSERT"))))      ;"newblocks"=list of blocks including the markers
;  (setq newnum (sslength newblocks))    
;  (princ newnum)
)