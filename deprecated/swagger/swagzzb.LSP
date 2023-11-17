(defun c:swagzzb ()
 ;duplicate
  (setq SUZY (ssget "X" '((0 . "LINE,LWPOLYLINE,CIRCLE,INSERT")))) ;makes "SUZY" a set of entities of these types
  (setq COUNTER 0)
  (setq DU (ssadd))  
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
    (setq A (ssget "_A"))                                  ;"A"=list of all entities
    (setq B (ssget "_A" '((0 . "LWPOLYLINE"))))            ;"B","C" are lists of acceptable entities
    (setq C (ssget "_A" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))

    (setq n (sslength B))                                  ;"n"=number of B
    (setq i -1)                                            ;"i"=-1 (0 at beginning of list)
    (repeat n (ssdel (ssname B (setq i (1+ i))) A))        ;removes ID of i'th element in list "B" from list "A"
    (setq n (sslength C))                                  ;"n"=number of C
    (setq i -1)                                            ;"i"=-1 (0 at beginning of list)
    (repeat n (ssdel (ssname C (setq i (1+ i))) A))        ;removes ID of i'th element in list "C" from list "A"

  ;2node except non-training layers-------------------------------------------------------
    (setq TN (ssget "_A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))
    (command "_LAYTHW")

  ;printout--------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")
    (print "Non-By-Layers Fixed per Output Above ^^")
    (print "Duplicates:         ") (princ (sslength DU))
        (setq n (sslength DU))                               ;"n"=number of duplicates
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname DU (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ". ")
          (princ (cdr (assoc 8 (entget CURRENT)))))           ;searches within the entity info for value type 8 (layer)
    (print "Unclosed Polylines: ") (princ (sslength UC))
        (setq n (sslength UC))                               ;"n"=number of uncloseds
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname UC (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ". ")
          (princ (cdr (assoc 8 (entget CURRENT)))))           ;searches within the entity info for value type 8 (layer)
    (print "Bad-DXF-Types:      ") (princ (sslength A))
        (setq n (sslength A))                               ;"n"=number of badtypes
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname A (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ". ")
          (princ (cdr (assoc 8 (entget CURRENT))))           ;searches within the entity info for value type 8 (layer)
        )
    (print "2-Node Polys:       ") (princ (sslength TN))
        (setq n (sslength TN))                               ;"n"=number of 2nodes
        (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
        (repeat n
          (setq CURRENT (ssname TN (setq i (1+ i))))         ;"CURRENT"=name of entity currently being evaluated
          (princ " ")(princ (+ 1 i))(princ ". ")
          (princ (cdr (assoc 8 (entget CURRENT))))           ;searches within the entity info for value type 8 (layer)
        )
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