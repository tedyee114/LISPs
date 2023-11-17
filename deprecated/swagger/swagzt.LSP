(defun c:swagzt ()
  ;non-by-layer---------------------------------------------------------------------------
    ;see below
  
  ;duplicate
        ;long as code snippet here compare to dummy lists
   

  ;uncloseds------------------------------------------------------------------
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
  
  ;bad-dxf-types except conc-pads, manholes, catch-------------------------------
    (setq BD (ssget "_A" '((0 . "LWPOLYLINE"))))                ;"A","B","C" are lists of acceptable entities
    (setq BB (ssget "_A" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))
    (setq n (sslength BB))                                     ;"n"=number of B
    (setq i -1)                                               ;"i"=-1 (0 at beginning of list)
    (repeat n (ssadd (ssname BB (setq i (1+ i))) BA))           ;appends ID of element #i in list B to list A    

  ;2node except non-training layers-------------------------------------------------------
    (setq TN (ssget "_A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))
    (command "_LAYTHW")

  ;printout--------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")
    (print "Non-By-Layers Fixed per Line Above ^")
    (print "Duplicates:         ") ;(princ GR)
    (print "Unclosed Polylines: ") (princ (sslength UC))
    (print "Bad-DXF-Types:      ") (princ (sslength BD))
    (print "2-Node Polys:       ") (princ (sslength TN))
    (print "GEOS-R Errors undetectable")
    ;(command "PSELECT" BD "")                      
    ;(command "HIDEOBJECTS")
)