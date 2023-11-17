(defun C:bduzd ()
  (setq good (ssget "X" '((0 . "CIRCLE,LWPOLYLINE,INSERT"))))    ;"good"=list of cirlces, polylines, blocks
  (setq ANNA (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))

  
  ;(setq polys (ssget "X" '((0 . "LWPOLYLINE"))))               ;"polys"=list of all polylines
  ;(setq circs (ssget "X" '((0 . "CIRCLE"))))                   ;"circs"=list of all circles
  ;(setq MH    (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR"))))    ;"MH"=list of circular manholes
  ;(setq CB    (ssget "X" '((0 . "CIRCLE") (8 . "U-CATCHBASIN-TR")))) ;"CB"=list of circular catchabsins
  ;(setq CP    (ssget "X" '((0 . "CIRCLE") (8 . "C-CONC-PAD"))))      ;"CP"=list of circular concrete pads


  (setq n (sslength ANNA))                                   ;"n"=number of circles
  (setq i -1)                                                  ;"i"=-1 (beginning of list)
  (repeat n (ssdel (ssname ANNA (setq i (1+ i))) good))     ;appends ID of element referenced by i in list circs to list polys

  (command "PSELECT" good "")                          ;selects, then isolates "polys", which now includes good circles
  (command "HIDEOBJECTS")
)