(defun c:swagp ()
  (princ "Working. This should take a few seconds...")
  ;duplicates - creates entity selection set DU-------------------------------------------------------------------------------------------------------------------------
    (setq SUZY (ssget "X" '((0 . "LWPOLYLINE,CIRCLE"))))                 ;"SUZY"=entity selection set containing all ("X") LWPOLYLINES and CIRCLES
    (setq COUNTER 0)                                                     ;"COUNTER"=0, used to iterate through every object below
    (setq DU (ssadd))                                                    ;creates "DU", the entity selection set that holds one of every duplicate entity
    (setq LWPOLYLINELIST  (ssadd))                                       ;creates empty entity selection set
    (setq CIRCLELIST      (ssadd))                                       ;creates empty entity selection set
    
    (while                                  ;this block adds entities to selection sets
      (setq EDNA (ssname SUZY COUNTER))                                  ;"EDNA"= value COUNTER'th element in list"SUZY"
      (setq ED (entget EDNA))                                            ;"ED"=attributes of entity EDNA
      (cond                                                              ;condition of while function
       ((= (cdr (assoc 0 ED)) "LWPOLYLINE") (ssadd EDNA LWPOLYLINELIST)) ;if layername found within of "ED" (attribute list of EDNA) is "LWPOLYLINE", add EDNA to the list "LWPOLYLINELIST"
       ((= (cdr (assoc 0 ED)) "CIRCLE")     (ssadd EDNA CIRCLELIST)))    ;if layername found within of "ED" (attribute list of EDNA) is "LWPOLYLINE", add EDNA to the list "CIRCLELIST"
      (setq COUNTER (1+ COUNTER))                                        ;increases counter
    )
    
    (setq entityType LWPOLYLINELIST)  (findit)                           ;changes the search area from one entity type to the next
    (setq entityType CIRCLELIST)      (findit)

  ;uncloseds - creates entity selection set UC-------------------------------------------------------------------------------------------------------------------------
    (command "LAYER" "FREEZE" "R-PAVEMARK-LINE" "")                      ;freezes non-training layers
    (command "LAYER" "FREEZE" "GEOM-CHECKER-ARCHIVE" "")
    (command "LAYER" "FREEZE" "U-MANHOLE" "")
    (command "LAYER" "FREEZE" "U-CATCHBASIN" "")
    (command "LAYER" "FREEZE" "A-OBSTRUCTION" "")
    (command "LAYER" "FREEZE" "Contour_Line_Intermediate" "")
    (command "LAYER" "FREEZE" "Contour_Line_Major" "")
    (command "LAYER" "FREEZE" "G-TOPO-MINR" "")
    (command "LAYER" "FREEZE" "G-TOPO-MAJR" "")
    (setq UC (ssget "A" '((0 . "LWPOLYLINE") (70 . 0))))                 ;"UC"=entity selection set of all unclosed polylines on unfrozen ("A") layers
  
  ;bad-dxf-entity-types - creates entity selection set A----------------------------------------------------------------------------------
    (setq A (ssget "A"))                                                 ;"A"=entity selection set of all unfrozen ("A") entities
    (setq B (ssget "A" '((0 . "LWPOLYLINE"))))                           ;"B","C" are entity selection sets of acceptable entities
    (setq C (ssget "A" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD,0"))))
                  ;these loops remove the acceptable entities from list "A"
    (setq n (sslength B))                                                ;"n"=number of B
    (setq i -1)                                                          ;"i"=-1 (0 at beginning of list)
    (repeat n (ssdel (ssname B (setq i (1+ i))) A))                      ;removes ID of i'th element in list "B" from list "A", repeats until it has worked n number of times
    (setq m (sslength C))                                                ;"n"=number of C
    (setq j -1)                                                          ;"i"=-1 (0 at beginning of list)
    (repeat m (ssdel (ssname C (setq j (1+ j))) A))                      ;removes ID of i'th element in list "C" from list "A", repeats until it has worked n number of times

  ;2nodes - creates entity selection set TN--------------------------------------------------------------------------------------------------
    (setq TN (ssget "A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))     ;"TN"= entity selection set of all unfrozen ("A") polylines
    (command "_LAYTHW")                                                  ;thaws (unfreezes) all layers

  ;printout--------------------------------------------------------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")                         ;sets all layer colors to "By-Layer"
    (print "Non-By-Layers Fixed per Output Above ^^")
    (print "Duplicates:                ") (setq inputlist DU) (printlist)
    (print "Unclosed Polylines:        ") (setq inputlist UC) (printlist)
    (print "Bad-DXF-Entity-Types:      ") (setq inputlist A)  (printlist)
    (print "2-Node Polys:              ") (setq inputlist TN) (printlist)
    (print "GEOS-R Errors undetectable")
    (setq Sapi (vlax-create-object "Sapi.SpVoice")) (vlax-invoke Sapi "Speak" "urrr" 0) (vlax-release-object Sapi) ;plays sound when command is done
    (princ)                                                              ;just a cleaner way to end a function (otherwise it ptints stuff you don't always want)
)

(defun findit()                                                          ;subfunction for finding duplicates
  (if (/= EntityType nil)                                                ;continue only if entity selection set "EntityType" is not nil (empty). Empty sets return nil, not 0.
    (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
     (setq ALPHA 0)                                                      ;"ALPHA" is a counter used to index the loop that specifies ED
                     
                ;sets ED, checks it with the contained while loop, then changes ED to the next entity in the list
      (while (setq ED (ssname entityType ALPHA))                         ;until ED returns nil (i.e. no entity selected), "ED"=ID of ALPHA'th entity in list "ENTITYTYPE"
        (setq EDattributes (cddddr (entget ED)))                         ;"EDattributes"=attributes of entity speicied by "ED", but not the first 4 items (cddddr is LISP syntax for "everything except the first 4 elements")
        (setq BETA 1)                                                    ;"BETA" is a counter used to index the loop that species EDNEXT
       
                ;compares info of EDattributes and EDNEXTattributes. If so, adds EDNEXT to the list of duplicates
        (while (setq EDNEXT (ssname entityType (+ ALPHA BETA)))          ;until EDNEXT returns nil (i.e. no entity selected), "EDNEXT"=ID of (sum of ALPHA+BETA)'th entity in list "ENTITYTYPE", this starts as the entity after ED, but gets updated to iterate through every entity on the list (note: entities are removed once checked)
          (setq EDNEXTattributes (cddddr (entget EDNEXT)))               ;"EDNEXTattributes"=attributes of entity speicied by "EDNEXT", but not the first 4 items (cddddr is LISP syntax for "everything except the first 4 elements")
          (if (member EDattributes (list EDNEXTattributes))              ;if "EDattributes" is a member of the dummy list "EDNEXT" (really means do the attributes match, but they could be in different orders, have extra info, etc)
            (progn                                                       
              (ssadd EDNEXT DU)                                          ;then: add it to the duplicate list and remove it from the search field
              (ssdel EDNEXT entityType)
            )
            (progn (setq BETA (1+ BETA)))                                ;else: increase "BETA", which changes EDNEXT, eventually ED will have been tested against every possible value of EDNEXT (every other entity)
          )  
        )
        (setq ALPHA (1+ ALPHA))                                          ;once ED has been tested against every EDNEXT, increase ALPHA, which provides a new ED for testing
     )
   )
 )
)

(defun printlist (inputlist)
  (princ (setq o (sslength inputlist)))                                  ;sets "o" to the length of "inputlist" and then prints it
  (setq k -1)                                                            ;"k"=-1 (0 at beginning of list)
  (repeat o
    (setq CURRENT (ssname inputlist (setq k (1+ k))))                    ;updates k, then sets "CURRENT"=ID of k'th entity in list "inputlist"
    (princ " ")(princ (+ 1 i))(princ ".")                                ;formatting shmuck so that list gets printed as "1. B-OVERHANG, 2. R-CURB, etc"
    (princ (cdr (assoc 8 (entget CURRENT))))                             ;gets layername of CURRENT- searches within the entity info for value type 8 (layer)
  )
)