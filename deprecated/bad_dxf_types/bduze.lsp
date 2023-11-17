(defun C:bduze ()
  (command "_layer" "_off" "G-TOPO-MINR" "")
  (command "_layer" "_off" "G-TOPO-MAJR" "")
  (setq A (ssget "X" '((0 . "LWPOLYLINE,INSERT"))))       ;"good"=list of cirlces, polylines, blocks
  (setq B (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))

  
  ;(setq polys (ssget "X" '((0 . "LWPOLYLINE"))))               ;"polys"=list of all polylines
  ;(setq circs (ssget "X" '((0 . "CIRCLE"))))                   ;"circs"=list of all circles
  ;(setq MH    (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR"))))    ;"MH"=list of circular manholes
  ;(setq CB    (ssget "X" '((0 . "CIRCLE") (8 . "U-CATCHBASIN-TR")))) ;"CB"=list of circular catchabsins
  ;(setq CP    (ssget "X" '((0 . "CIRCLE") (8 . "C-CONC-PAD"))))      ;"CP"=list of circular concrete pads

  ;Appender Loop
  (setq n (sslength B))                                   ;"n"=number of B
  (setq i -1)                                             ;"i"=-1 (0 atbeginning of list)
  (repeat n (ssadd (ssname B (setq i (1+ i))) good))      ;appedelets ID of element referenced by i in list Bs to list A

  (command "PSELECT" A "")                                ;selects, then isolates list A, which now includes  B elements too
  (command "HIDEOBJECTS")
)