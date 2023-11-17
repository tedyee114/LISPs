(defun C:bdt ()
  (setq SUZY (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,INSERT")))) ;makes "SUZY" a set of all those entity types
  (setq COUNTER 0)
  (setq duplicatelist (ssadd))
  (setq numdups (sslength duplicatelist))
  
  (setq SSLINE        (ssadd))                          ;creating new selection sets for each entity type
  (setq SSLWPOLYLINE  (ssadd))
  (setq SSCIRCLE      (ssadd))
  (setq SSARC         (ssadd))
  (setq SSELLIPSE     (ssadd))
  (setq SSSPLINE      (ssadd))
  (setq SSINSERT      (ssadd))
  
  (while 
    (setq EDNA (ssname SUZY COUNTER))                    ;sets value of EDNA to the value of SUZY at the index location of counter
    (setq ED (entget EDNA))                              ;sets value of ED to the entity info of EDNA
    (cond                                                ;condition of while function
     ((= (dxf 0 ED) "LINE")       (ssadd EDNA SSLINE))
     ((= (dxf 0 ED) "LWPOLYLINE") (ssadd EDNA SSLWPOLYLINE))
     ((= (dxf 0 ED) "CIRCLE")     (ssadd EDNA SSCIRCLE))
     ((= (dxf 0 ED) "ARC")        (ssadd EDNA SSARC))
     ((= (dxf 0 ED) "ELLIPSE")    (ssadd EDNA SSELLIPSE))
     ((= (dxf 0 ED) "SPLINE")     (ssadd EDNA SSSPLINE))
     ((= (dxf 0 ED) "INSERT")     (if (not (ASSOC 66 ED)) (ssadd EDNA SSINSERT)))
     (t nil) 
    )
    (setq COUNTER (+ 1 COUNTER))
  )
  
  (setq entityType SSLINE)        (findit)               ;changes the search area from one entity type to the next
  (setq entityType SSLWPOLYLINE)  (findit)
  (setq entityType SSCIRCLE)      (findit)
  (setq entityType SSARC)         (findit)
  (setq entityType SSELLIPSE)     (findit)
  (setq entityType SSSPLINE)      (findit)
  (setq entityType SSINSERT)      (findit)
  
  (print "Number of Total Duplicates:")                        ;The end of the code
  (command "PSELECT" DUPLICATELIST "")   
  (command "ISOLATEOBJECTS")
)
;--------------------------------------------------------------------------------------------
(defun dxf (n ed) (cdr (assoc n ed)))                    ;function "dxf" needs inputs "n" and "ED", takes all but 1st info searhes nth element in list "ED"
;--------------------------------------------------------------------------------------------
(defun finder()
  (setq LENGTHEntityType (sslength EntityType))          
  (if (> LENGTHEntityType 1)                             ;skip if there are no entities of that type
    (progn
     (setq COUNTER 0)
     (while (setq EDNA (ssname entityType COUNTER))      ;"EDNA"=name of entity specified as element at location "COUNTER" in list "EntityType"
        (setq ED (cddddr (entget EDNA)))                 ;"ED"=5th attribute of entity, entity specified as the one named by EDNA (cddddr comes from LISP)
        (setq COUNTERNEXT 1)
        (while (setq EDNANEXT (ssname  entityType (+ COUNTER COUNTERNEXT)))   ;"EDNANEXT"=name of entity specified by location "COUNTER+1" in list "EntityType"
          (setq NAMY (dxf 0 (entget EDNANEXT)))          ;"NAMY"=output of the fn "dxf" with inputs of n=0 and ED=(attributes of EDNANEXT)
          (setq EDNEXT (cddddr (entget EDNANEXT)))       ;"EDNEXT"=5th attribute of entity, entity specified as the one named by EDNANEXT (cddddr comes from LISP)
          (if  (member ED (list EDNEXT))                 ;if "ED" is a member of list "EDNEXT" (i.e. if an entity has same 5th attribute as another entity in a list containing everything except itself)
            (progn                                       ;outputs to user here
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