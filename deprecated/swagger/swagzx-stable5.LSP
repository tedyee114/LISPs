(defun c:swagzx ()
 ;duplicate
  (setq SUZY (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,INSERT")))) ;makes "SUZY" a set of entities of these types
  (setq COUNTER 0)
  (setq duplicatelist (ssadd))  
  (setq SSLINE        (ssadd))                              ;creating new selection sets for each entity type
  (setq SSLWPOLYLINE  (ssadd))
  (setq SSCIRCLE      (ssadd))
  (setq SSINSERT      (ssadd))                              ;INSERT is for blocks
  
  (while 
    (setq EDNA (ssname SUZY COUNTER))                       ;sets value of EDNA to the value of SUZY at the index location of counter
    (setq ED (entget EDNA))                                 ;sets value od ED to the entity info of EDNA
    (cond                                                   ;condition of while function
     ((= (dxf 0 ED) "LINE")       (ssadd EDNA SSLINE))
     ((= (dxf 0 ED) "LWPOLYLINE") (ssadd EDNA SSLWPOLYLINE))
     ((= (dxf 0 ED) "CIRCLE")     (ssadd EDNA SSCIRCLE))
     ((= (dxf 0 ED) "INSERT")     (if (not (ASSOC 66 ED)) (ssadd EDNA SSINSERT)))
     (t nil) 
    )
    (setq COUNTER (+ 1 COUNTER))
  )
  
  (setq entityType SSLINE)        (findit)                  ;changes the search area from one entity type to the next
  (setq entityType SSLWPOLYLINE)  (findit)
  (setq entityType SSCIRCLE)      (findit)
  (setq entityType SSINSERT)      (findit)

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
    (setq allTRlayers (ssget "_A"))
    (setq goods (ssget "_A" '((0 . "LWPOLYLINE"))))          ;"goods","others" are lists of acceptable entities
    (setq others (ssget "_A" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))
    (setq n (sslength others))                               ;"n"=number of others
    (setq i -1)                                              ;"i"=-1 (0 at beginning of list)
    (repeat n (ssadd (ssname others (setq i (1+ i))) goods)) ;appends ID of i'th element in list "others" to list "goods"

  ;2node except non-training layers-------------------------------------------------------
    (setq TN (ssget "_A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))
    (command "_LAYTHW")

  ;printout--------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")
    (print "Non-By-Layers Fixed per Output Above ^^")
    (print "Duplicates:         ") (princ (sslength DUPLICATELIST))
    (print "Unclosed Polylines: ") (princ (sslength UC))
    (print "Bad-DXF-Types:      ") (princ (- (sslength allTRlayers) (sslength goods)))
    (print "2-Node Polys:       ") (princ (sslength TN))
    (print "GEOS-R Errors undetectable")
)

(defun dxf (n ed) (cdr (assoc n ed)))                       ;subfunction "dxf" needs inputs "n" and "ED". searches for nth element in list "ED" and takes all but 1st attibute
(defun findit()
  (setq LENGTHEntityType (sslength EntityType))          
  (if (> LENGTHEntityType 1)                                ;skip if there are no entities of that type
    (progn
     (setq COUNTER 0)
     (while (setq EDNA (ssname  entityType COUNTER))        ;defines value of "EDNA" as the name of the object specified by location "COUNTER" in list "EntityType"
        (setq ED (cddddr (entget EDNA)))                    ;defines value of "ED" as data taken from EDNA, but not the first 4 items (cddddr comes from LISP)
        (setq COUNTERNEXT 1)
        (while (setq EDNANEXT (ssname  entityType (+ COUNTER COUNTERNEXT)))   ;defines value of "EDNANEXT" as the name of the object specified by location "COUNTER" in list "EntityType"
          (setq NAMY (dxf 0 (entget EDNANEXT)))             ;defines value of "NAMY" as the output of the fn "dxf" with inputs of n=0 and ED=EDNANEXT
          (setq EDNEXT (cddddr (entget EDNANEXT)))          ;compares lists ED and EDNEXT, asks if the first list is member of the second list
          (if  (member ED (list EDNEXT))                    ;if "ED" is a member of the dummy list "EDNEXT"
            (progn                                          ;print an output of the type, add it to the duplicate list and remove it from the search field
              (print "Duplicate found. TYPE:")(princ NAMY)
              (ssadd EDNANEXT duplicatelist)
              (ssdel EDNANEXT entityType)
            )
            (PROGN
              (setq COUNTERNEXT (+ 1 COUNTERNEXT))
            )
          )  
        )
        (setq COUNTER (+ 1 COUNTER))
     )
   )
 )
  (princ)
)