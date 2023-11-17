(defun C:bduf ()
  (setq polys (ssget "X" '((0 . "LWPOLYLINE"))))               ;"polys"=list of polylines
  (setq circs (ssget "X" '((0 . "CIRCLE"))))                   ;"circs"=list of circles

  (setq numcircs (sslength circs))                             ;"numcircs"=number of circles
  (setq counter -1)                                            ;"counter"=-1 (beginning of list)
  (repeat numcircs                                             ;repeat as many times as there are circles
    (setq current (ssname circs (setq counter (1+ counter))))  ;"current"=ID of circle at location "counter" in list "circs"
    (ssadd current polys)                                      ;appends the ID of current circle to list "polys"
  )
  
  ;;  (while (>= numcircs counter)                             ;repeat until the counter value = number of circles (end of list)
  ;;   (setq current (ssname circs counter))                   ;"current"=ID of circle at location "counter" in list "circs"
  ;;   (ssadd current polys)                                   ;appends the ID of current circle to list "polys"
  ;;   (setq counter (+ 1 counter))                            ;increases "counter"
  ;; ) 

  (command "PSELECT" polys "")                          ;selects, then isolates "polys", which now includes good circles
  (command "ISOLATEOBJECTS")
)