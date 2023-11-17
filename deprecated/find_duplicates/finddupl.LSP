(defun C:finddupl ()
  (setq SUZY (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,INSERT")))) ;makes "SUZY" a set of all those entity types
  (setq LENSUZY (sslength SUZY))
  (setq COUNTER 0)
  (setq SSLINE       (ssadd))               ;creating new selection sets for each entity type
  (setq SSLWPOLYLINE (ssadd))
  (setq SSCIRCLE     (ssadd))
  (setq SSARC        (ssadd))
  (setq SSELLIPSE    (ssadd))
  (setq SSSPLINE     (ssadd))
  (setq SSINSERT     (ssadd))
  
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
  
  (setq SSWHICH SSLINE)        (purgeType) ;changes search area from one entity type to next, then runs below function called "purgeType"
  (setq SSWHICH SSLWPOLYLINE)  (purgeType)
  (setq SSWHICH SSCIRCLE)      (purgeType)
  (setq SSWHICH SSARC)         (purgeType)
  (setq SSWHICH SSELLIPSE)     (purgeType)
  (setq SSWHICH SSSPLINE)      (purgeType)
  (setq SSWHICH SSINSERT)      (purgeType)
)
;--------------------------------------------------------------------------------------------
(defun dxf (n ED))                         ;the function here defined as "dxf" requires inputs for "n" and "ED"
;--------------------------------------------------------------------------------------------
(defun purgeType()
(setq LENGTHSSWHICH (sslength SSWHICH))    ;tells it to set the countdown to the number of objects of that type
  (if (> LENGTHSSWHICH 1)                  ;as long as there is an object of that type, continue
    (progn
      (setq COUNTER 0)
      (while (setq EDNA (ssname  SSWHICH COUNTER))  ;defines value of "EDNA" to be the name of the object specified by location "COUNTER" in list "SSWHICH"
        (setq ED (cddddr (entget EDNA)))            ;cddddr comes from LISP and just means cut off the first 4 pieces of data taken from EDNA
        (while (setq EDNANEXT (ssname  SSWHICH (COUNTER) )) ;value of EDNA next is 
          (setq NAMY (dxf 0 (entget EDNANEXT)))
          (setq EDNEXT (cddddr (entget EDNANEXT))) ;compares lists ED and EDNEXT, asks if the first list is member of the second list
          (if  (member ED (list EDNEXT))
            (progn
              (print "Duplicate ")(princ NAMY)
              (command "SELECT" EDNANEXT)                            ;used to delete entity here, now seletcs
              (ssdel EDNANEXT SSWHICH)
            )
            (PROGN
            (setq COUNTER (+ 1 COUNTER))
            )
          )  
        )
       
        (setq COUNTER (+ 1 COUNTER))  ;(print ED)
      );END while
    )
  );END if
  (princ)
)