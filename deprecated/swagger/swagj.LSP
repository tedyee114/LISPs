(defun c:swagj ()
 ;duplicate
        ;long as code snippet here compare to dummy lists
   

 ;uncloseds------------------------------------------------------------------
        ; (ssget "X" '((0 . "LWPOLYLINE") (70 . 0)))

 ;non-by-layer---------------------------------------------------------------------------
        ;see below


;;  ;bad-dxf-types except conc-pads, manholes, catch-------------------------------
;;   (command "_layer" "_off" "G-TOPO-MINR" "")
;;   (command "_layer" "_off" "G-TOPO-MAJR" "")
  
;;   (setq A (ssget "X" '((0 . "LWPOLYLINE"))))                ;"A","B","C" are lists of acceptable entities
;;   (setq B (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))
;;   (setq C (ssget "X" '((0 . "INSERT") (8 . "U-MANHOLE,U-CATCHBASIN"))))
  
;;  ;Appender Loops
;;   (setq n (sslength B))                                     ;"n"=number of B
;;   (setq i -1)                                               ;"i"=-1 (0 at beginning of list)
;;   (repeat n (ssadd (ssname B (setq i (1+ i))) A))           ;appends ID of element #i in list B to list A
  
;;   (setq o (sslength C))                                   
;;   (setq j -1)                                             
;;   (repeat o (ssadd (ssname C (setq j (1+ j))) A))         

;;   (command "PSELECT" A "")                                  ;isolates list A, which now includes B,C elements too
;;   (command "HIDEOBJECTS")
;;   (setq BD (ssget "ALL"))
  

 ;2node except non-training layers-------------------------------------------------------
   (command "_layer" "_off" "R-PAVEMARK-LINE" "")
   (command "_layer" "_off" "GEOM-CHECKER-ARCHIVE" "")
   (command "_layer" "_off" "U-MANHOLE" "")
   (command "_layer" "_off" "U-CATCHBASIN" "")
   (command "_layer" "_off" "A-OBSTRUCTION" "")
   (command "_layer" "_off" "G-TOPO-MINR" "")
   (command "_layer" "_off" "G-TOPO-MAJR" "") 
   (setq TN (ssget "ALL" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)
                         
                         )))
 ;printout--------------------------------------------------------------------------
  (print "Duplicates: ")         ;(princ GR)
  (print "Unclosed Polylines: ") ;(princ GR)
  (print "Non-By-Layers Fixed:") ;(command "_SetByLayer" "ALL" "" "YES" "Yes")
  (print "Bad-DXF-Types: ")      (princ (sslength BD))
  (print "2-Node Polys: ")       (princ (sslength TN))
  (print "Use GER command layer by layer for GEOS-R Errors")
)