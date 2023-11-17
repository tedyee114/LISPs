(defun C:bdtw ()
  (command "_layer" "_off" "U-MANHOLE" "")
  (command "_layer" "_off" "U-CATCHBASIN" "")
  ;(setq stuff (ssget "X"))                             ;"stuff:"=everything

  (setq polys  (ssget "X" '((0 . "LWPOLYLINE"))))       ;"polys"=list of polylines
  ;lock circular layers
  (setq circs  (ssget ":L" '((0 . "CIRCLES"))))         ;"circs"=list circles on unlocked layers 
    
  (setq numcircs (sslength circs))                      ;"numcircs"=number of circles
  (setq counter=0)                                      ;"counter"=0 (beginning of list)
  (while (>= numcircs counter)                          ;repeat until the counter reaches the end of the list
    (setq current (ssname circs counter))               ;"current"=ID of circle at location "counter" in list "circs"
    (ssadd current polys)                               ;adds the ID of current circle to list "polys"
    (setq counter (1+ counter))                         ;increases "counter"
  ) 

  (command "PSELECT" polys "")                          ;selects, then isolates "polys", which now includes good circles
  (command "ISOLATEOBJECTS")
)

;; (defun ssunion (ss1 ss2 / n1 n2 i)
;; (setq i -1)
;; (if (< (setq n1 (sslength ss1)) (setq n2 (sslength ss2)))
;; (repeat n1 (ssadd (ssname ss1 (setq i (1+ i))) ss2))
;; (repeat n2 (ssadd (ssname ss2 (setq i (1+ i))) ss1))
;; )
;; )