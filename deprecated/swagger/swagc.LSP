(defun c:swagc ()
 ;duplicate
        ;long as code snippet here compare to dummy lists
   

 ;uncloseds------------------------------------------------------------------
        ; (ssget "X" '((0 . "LWPOLYLINE") (70 . 0)))

 ;non-by-layer---------------------------------------------------------------------------
        ;see below


 ;bad-dxf-types except conc-pads, manholes, catch-------------------------------
  (command "_layer" "_off" "G-TOPO-MINR" "")
  (command "_layer" "_off" "G-TOPO-MAJR" "")
  
  (setq A (ssget "X" '((0 . "LWPOLYLINE"))))                ;"A","B","C" are lists of acceptable entities
  (setq B (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))
  (setq C (ssget "X" '((0 . "INSERT") (8 . "U-MANHOLE,U-CATCHBASIN"))))
  
 ;Appender Loops-----------------------------------------------------------------
  (setq n (sslength B))                                     ;"n"=number of B
  (setq i -1)                                               ;"i"=-1 (0 at beginning of list)
  (repeat n (ssadd (ssname B (setq i (1+ i))) A))           ;appends ID of element #i in list B to list A
  
  (setq o (sslength C))                                   
  (setq j -1)                                             
  (repeat o (ssadd (ssname C (setq j (1+ j))) A))         

  (command "PSELECT" A "")                                  ;isolates list A, which now includes B,C elements too
  (command "HIDEOBJECTS")
  (setq BD (ssget "a"))
  

 ;2node        except non-training layers--------------------------------------------
  (setq TN (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))
  
 ;geos-r detector
 ; (setq blocks (ssget "X" '((0 . "INSERT"))))                ;"blocks"=list of blocks
;  (command "_mapclean" "gera")                               ;places a block at the intersections
 ; (setq newblocks (ssget "X" '((0 . "INSERT"))))             ;"newblocks"=list of blocks including the markers
 ; (setq GR (-(sslength blocks)(sslength newblocks)))            ;"detected" value reflects whether the number of blocks has changed
    
 ;printout--------------------------------------------------------------------------
  (print "Duplicates: ");(princ GR)
  (print "Unclosed Polylines: ");(princ GR)
  (print "Non-By-Layers Fixed:")
  (command "_SetByLayer" "ALL" "" "YES" "Yes")
  (print "Bad-DXF-Types: ")(princ GR)
  (print "2-Node Polys: ")(princ TN)
  (print "GEOS-R Errors: ");(princ GR)
)