(defun c:swagzc ()
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
    (command "_layer" "_freeze" "Contour_Line_Intermedite" "")
    (command "_layer" "_freeze" "Contour_Line_Major" "")
    (command "_layer" "_freeze" "G-TOPO-MINR" "")
    (command "_layer" "_freeze" "G-TOPO-MAJR" "")
    (setq UC (ssget "_A" '((0 . "LWPOLYLINE") (70 . 0))))

  ;bad-dxf-types except conc-pads, manholes, catch-------------------------------
    (setq A (ssget "X" '((0 . "LWPOLYLINE"))))                ;"A","B","C" are lists of acceptable entities
    (setq B (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))
    (setq C (ssget "X" '((0 . "INSERT") (8 . "U-MANHOLE,U-CATCHBASIN"))))
  
    (setq n (sslength B))                                     ;"n"=number of B
    (setq i -1)                                               ;"i"=-1 (0 at beginning of list)
    (repeat n (ssadd (ssname B (setq i (1+ i))) A))           ;appends ID of element #i in list B to list A

    (setq o (sslength C))                                   
    (setq j -1)                                             
    (repeat o (ssadd (ssname C (setq j (1+ j))) A))         

  ;2node except non-training layers-------------------------------------------------------
    (setq TN (ssget "_A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))
    ;(command "_LAYTHW")

  ;printout--------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")
    (print "Non-By-Layers Fixed per Line Above ^")
    (print "Duplicates:         ") ;(princ GR)
    (print "Unclosed Polylines: ") (princ (sslength UC))
    (print "Bad-DXF-Types:      ") (princ (sslength A))
    (print "2-Node Polys:       ") (princ (sslength TN))
    (print "Use GER command layer by layer for GEOS-R Errors")
)