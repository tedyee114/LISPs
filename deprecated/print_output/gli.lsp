(defun c:gli ()
    (command "_layer" "_freeze" "R-PAVEMARK-LINE" "")
    (command "_layer" "_freeze" "GEOM-CHECKER-ARCHIVE" "")
    (command "_layer" "_freeze" "U-MANHOLE" "")
    (command "_layer" "_freeze" "U-CATCHBASIN" "")
    (command "_layer" "_freeze" "A-OBSTRUCTION" "")
    (command "_layer" "_freeze" "Contour_Line_Intermediate" "")
    (command "_layer" "_freeze" "Contour_Line_Major" "")
    (command "_layer" "_freeze" "G-TOPO-MINR" "")
    (command "_layer" "_freeze" "G-TOPO-MAJR" "")
    (setq UC (ssget "_A" '((0 . "LWPOLYLINE") (70 . 0))))
    (setq n (sslength UC))                               ;"n"=number of UC
    (setq i -1)                                              ;"i"=-1 (0 at beginning of list)
    (repeat n 
      (setq CURRENT (ssname UC (setq i (1+ i))))
      ;(print "layer name:")
      (princ (cdr (assoc 8 (entget CURRENT))))
    )    
)