(defun c:swagj ()
  (princ "Working. This should take a few seconds...")
  ;duplicates--------------------------------------------------------------------------------------------------------------------------
    (setq SUZY (ssget "X" '((0 . "LWPOLYLINE,CIRCLE"))))                 ;"SUZY"=entity selection set containing all ("X") LWPOLYLINES and CIRCLES
    (setq COUNTER 0)                                                     ;"COUNTER"=0, used to iterate through every object below
    (setq DU (ssadd))                                                    ;creates "DU", the entity selection set that holds one of every duplicate entity
    (setq LWPOLYLINELIST  (ssadd))                                       ;creates empty entity selection set
    (setq CIRCLELIST      (ssadd))                                       ;creates empty entity selection set
    
    (while                                  ;this block adds entities to selection sets
      (setq EDNA (ssname SUZY COUNTER))                                  ;"EDNA"= value COUNTER'th element in list"SUZY"
      (setq ED (entget EDNA))                                            ;"ED"=attributes of entity EDNA
      (cond                                                              ;condition of while function
       ((= (cdr (assoc 0 ED)) "LWPOLYLINE") (ssadd EDNA LWPOLYLINELIST)) ;if layername ofthe output of subfunction "dxf" with inputs n=0 and  ed=ED is "LWPOLYLINE", add EDNA to the list "LWPOLYLINELIST"
       ((= (cdr (assoc 0 ED)) "CIRCLE")     (ssadd EDNA CIRCLELIST)))    ;if the output of subfunction "dxf" with inputs n=0 and  ed=ED is "CIRCLE", add EDNA to the list "CIRCLELIST"
      (setq COUNTER (1+ COUNTER))                                        ;increases counter
    )
    
    (setq entityType LWPOLYLINELIST)  (findit)                           ;changes the search area from one entity type to the next
    (setq entityType CIRCLELIST)      (findit)

  ;uncloseds------------------------------------------------------------------------------------------------------------------------
    (command "_layer" "_freeze" "R-PAVEMARK-LINE" "")                    ;freezes non-training layers
    (command "_layer" "_freeze" "GEOM-CHECKER-ARCHIVE" "")
    (command "_layer" "_freeze" "U-MANHOLE" "")
    (command "_layer" "_freeze" "U-CATCHBASIN" "")
    (command "_layer" "_freeze" "A-OBSTRUCTION" "")
    (command "_layer" "_freeze" "Contour_Line_Intermediate" "")
    (command "_layer" "_freeze" "Contour_Line_Major" "")
    (command "_layer" "_freeze" "G-TOPO-MINR" "")
    (command "_layer" "_freeze" "G-TOPO-MAJR" "")
    (setq UC (ssget "A" '((0 . "LWPOLYLINE") (70 . 0))))                ;"UC"=entity selection set of all unclosed polylines on unfrozen ("A") layers
  
  ;bad-dxf-types except conc-pads, manholes, catch----------------------------------------------------------------------------------
    (setq A (ssget "A"))                                  ;"A"=entity selection set of all unfrozen ("A"entities
    (setq B (ssget "A" '((0 . "LWPOLYLINE"))))            ;"B","C" are lists of acceptable entities
    (setq C (ssget "A" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD,0"))))

    (setq n (sslength B))                                  ;"n"=number of B
    (setq i -1)                                            ;"i"=-1 (0 at beginning of list)
    (repeat n (ssdel (ssname B (setq i (1+ i))) A))        ;removes ID of i'th element in list "B" from list "A"
    (setq n (sslength C))                                  ;"n"=number of C
    (setq i -1)                                            ;"i"=-1 (0 at beginning of list)
    (repeat n (ssdel (ssname C (setq i (1+ i))) A))        ;removes ID of i'th element in list "C" from list "A"

  ;2node except non-training layers--------------------------------------------------------------------------------------------------
    (setq TN (ssget "A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))
    (command "_LAYTHW")

  ;printout--------------------------------------------------------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")
    (print "Non-By-Layers Fixed per Output Above ^^")
    (print "Duplicates:         ") (princ (sslength DU))
        (setq n (sslength DU))                               ;"n"=number of duplicates
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname DU (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ".")
          (princ (cdr (assoc 8 (entget CURRENT)))))           ;searches within the entity info for value type 8 (layer)
    (print "Unclosed Polylines: ") (princ (sslength UC))
        (setq n (sslength UC))                               ;"n"=number of uncloseds
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname UC (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ".")
          (princ (cdr (assoc 8 (entget CURRENT)))))           ;searches within the entity info for value type 8 (layer)
    (print "Bad-DXF-Types:      ") (princ (sslength A))
        (setq n (sslength A))                               ;"n"=number of badtypes
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname A (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ".")
          (princ (cdr (assoc 8 (entget CURRENT))))           ;searches within the entity info for value type 8 (layer)
        )
    (print "2-Node Polys:       ") (princ (sslength TN))
        (setq n (sslength TN))                               ;"n"=number of 2nodes
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname TN (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ".")
          (princ (cdr (assoc 8 (entget CURRENT))))           ;searches within the entity info for value type 8 (layer)
        )
    (print "GEOS-R Errors undetectable")
    (setq Sapi (vlax-create-object "Sapi.SpVoice")) (vlax-invoke Sapi "Speak" "urrr" 0) (vlax-release-object Sapi) ;plays sound when command is done
    (princ)
)

(defun findit()
  (setq LENGTHEntityType (sslength EntityType))          
  (if (> LENGTHEntityType 1)                                ;skip if there are no entities of that type
    (progn
     (setq COUNTER 0)
     (while (setq EDNA (ssname  entityType COUNTER))        ;defines value of "EDNA" as the name of the object specified by location "COUNTER" in list "EntityType"
        (setq ED (cddddr (entget EDNA)))                    ;defines value of "ED" as data taken from EDNA, but not the first 4 items (cddddr comes from LISP)
        (setq COUNTERNEXT 1)
        (while (setq EDNANEXT (ssname  entityType (+ COUNTER COUNTERNEXT)))   ;defines value of "EDNANEXT" as the name of the object specified by location "COUNTER" in list "EntityType"
          (setq NAMY (cdr (assoc 0 (entget EDNANEXT))))             ;defines value of "NAMY" as the output of the fn "dxf" with inputs of n=0 and ED=EDNANEXT
          (setq EDNEXT (cddddr (entget EDNANEXT)))          ;compares lists ED and EDNEXT, asks if the first list is member of the second list
          (if  (member ED (list EDNEXT))                    ;if "ED" is a member of the dummy list "EDNEXT"
            (progn                                          ;print an output of the type, add it to the duplicate list and remove it from the search field
              (print "Duplicate found. TYPE:")(princ NAMY)
              (ssadd EDNANEXT DU)
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