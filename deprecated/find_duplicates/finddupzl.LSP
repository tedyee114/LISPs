(defun C:finddupzl ()
  (setq SUZY (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,INSERT")))) ;makes "SUZY" a set of all those entity types
  (setq LENSUZY (sslength SUZY))
  (setq COUNTER 0)
  (setq duplicatelist (ssadd))
  (setq SSLINE        (ssadd))               ;creating new selection sets for each entity type
  (setq SSLWPOLYLINE  (ssadd))
  (setq SSCIRCLE      (ssadd))
  (setq SSARC         (ssadd))
  (setq SSELLIPSE     (ssadd))
  (setq SSSPLINE      (ssadd))
  (setq SSINSERT      (ssadd))
  
  (while 
    (setq EDNA (ssname SUZY COUNTER))       ;sets value of EDNA to the value of SUZY at the index location of counter
    (setq ED (entget EDNA))                 ;sets value od ED to the entity info of EDNA
    (cond                                   ;condition of while function
     ((= (dxf 0 ED) "LINE")     (ssadd EDNA SSLINE))
     ((= (dxf 0 ED) "LWPOLYLINE") (ssadd EDNA SSLWPOLYLINE))
     ((= (dxf 0 ED) "CIRCLE")     (ssadd EDNA SSCIRCLE))
     ((= (dxf 0 ED) "ARC")    (ssadd EDNA SSARC))
     ((= (dxf 0 ED) "ELLIPSE")    (ssadd EDNA SSELLIPSE))
     ((= (dxf 0 ED) "SPLINE")    (ssadd EDNA SSSPLINE))
     ((= (dxf 0 ED) "INSERT")    (if (not (ASSOC 66 ED)) (ssadd EDNA SSINSERT)))
     (t nil)
    )
    (setq COUNTER (+ 1 COUNTER))
  )
  
  (setq entityType SSLINE)        (findit) ;changes the search area from one entity type to the next
  (setq entityType SSLWPOLYLINE)  (findit)
  (setq entityType SSCIRCLE)      (findit)
  (setq entityType SSARC)         (findit)
  (setq entityType SSELLIPSE)     (findit)
  (setq entityType SSSPLINE)      (findit)
  (setq entityType SSINSERT)      (findit)
  
  (print "Number of Duplicates: (not correct)") (princ (sslength DUPLICATELIST))  ;The end of the code
  (command "PSELECT" "DUPLICATELIST")   
  ;(command "ISOLATEOBJECTS")
)
;--------------------------------------------------------------------------------------------
(defun dxf (n ed) (cdr (assoc n ed)))            ;the function here defined as "dxf" requires inputs for "n" and "ED"
;--------------------------------------------------------------------------------------------
(defun findit()
 (setq LENGTHEntityType (sslength EntityType))           ;tells it to set the countdown to the number of objects of that type
  (if (> LENGTHEntityType 1)                             ;skip if there are no entities of that type
    (progn
     (setq COUNTER 0)
     (while (setq EDNA (ssname  entityType COUNTER))     ;defines value of "EDNA" as the name of the object specified by location "COUNTER" in list "EntityType"
        (setq ED (cddddr (entget EDNA)))                 ;defines value of "ED" as data taken from EDNA, but not the first 4 items (cddddr comes from LISP)
        (setq COUNTERNEXT 1)
        (while (setq EDNANEXT (ssname  entityType (+ COUNTER COUNTERNEXT)))   ;defines value of "EDNANEXT" as the name of the object specified by location "COUNTER" in list "EntityType"
          (setq NAMY (dxf 0 (entget EDNANEXT)))          ;defines value of "NAMY" as the output of the fn "dxf" with inputs of n=0 and ED=EDNANEXT
          (setq EDNEXT (cddddr (entget EDNANEXT)))       ;compares lists ED and EDNEXT, asks if the first list is member of the second list
          (if  (member ED (list EDNEXT))                 ;if "ED" is a member of the dummy list "EDNEXT"
            (progn
              (print "Duplicate found. TYPE:")(princ NAMY)
              ;(command "SELECT" EDNANEXT)                ;used to delete entity here
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