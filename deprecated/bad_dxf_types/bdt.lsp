(defun C:bdt ()
  (setq SUZY (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,INSERT")))) ;makes "SUZY" a set of all those entity types
  (setq COUNTER 0)
  (setq badlist (ssadd))  
  (setq SSLINE        (ssadd))                              ;creating new selection sets for each entity type
  (setq SSLWPOLYLINE  (ssadd))
  (setq SSCIRCLE      (ssadd))
  (setq SSARC         (ssadd))
  (setq SSELLIPSE     (ssadd))
  (setq SSSPLINE      (ssadd))
  (setq SSINSERT      (ssadd))
  
  (while 
    (setq EDNA (ssname SUZY COUNTER))                       ;sets value of EDNA to the value of SUZY at the index location of counter
    (setq ED (entget EDNA))                                 ;sets value of ED to the entity info of EDNA
    (cond                                                   ;condition of while function
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

  (setq entityType SSLINE)        (finder)                  ;changes the search area from one entity type to the next
  (setq entityType SSLWPOLYLINE)  (finder)
  (setq entityType SSCIRCLE)      (finder)
  (setq entityType SSARC)         (finder)
  (setq entityType SSELLIPSE)     (finder)
  (setq entityType SSSPLINE)      (finder)
  (setq entityType SSINSERT)      (finder)
  
  (print "Number of Total Bad Entities: ")                  ;The end of the code
  (command "PSELECT" badlist "")   
  (command "ISOLATEOBJECTS")
)
;--------------------------------------------------------------------------------------------
(defun dxf (n ed) (cdr (assoc n ed)))                       ;function "dxf" needs inputs "n" and "ED", takes all but 1st info searhes nth element in list "ED"
;--------------------------------------------------------------------------------------------
(defun finder()
  (setq ENT (ssname entityType COUNTER))                    ;"ENT"=name of entity at position COUNTER in list entityType
  (setq TYP (cddddr (entget ENT)))                          ;"TYP"=5th attribute of ENT (entity type)
  (setq LAY (cdddr (entget ENT)))                           ;"TYP"=4th attribute of ENT (layer)
  (while
    (if (/= TYP "LWPOLYLINE, CIRCLE")                         ;if its a weird shape
      (ssadd ENT badlist))                                     ;add it to the bad list
    (if (/= LAY "C-CONC-PAD, U-MANHOLE-TR, U-CATCHBASIN-TR")  ;if its not a concpad, manhole-TR, cathbasin-TR
      (ssadd ENT badlist))                                     ;add it to the bad list
    (setq COUNTER (+ 1 COUNTER))
  )
) 