(defun C:bdui ()
  (setq polys (ssget "X" '((0 . "LWPOLYLINE"))))               ;"polys"=list of polylines
  (setq circs (ssget "X" '((0 . "CIRCLE"))))                   ;"circs"=list of circles

  (setq n1 (sslength circs))                                   ;"n1"=number of circles
  (setq i -1)                                                  ;"i"=-1 (beginning of list)
  (repeat n1 (ssadd (ssname circs (setq i (1+ i))) polys))     ;appends ID of element referenced by i in list circs to list polys

  (command "PSELECT" polys "")                          ;selects, then isolates "polys", which now includes good circles
  (command "ISOLATEOBJECTS")
)