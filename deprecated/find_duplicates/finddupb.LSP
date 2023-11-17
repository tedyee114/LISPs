(defun C:finddupb ()
(setq SUZY (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,INSERT"))))
(setq LENSUZY (sslength SUZY))
  (setq COUNTER 0)
  
     (setq SSLINE (ssadd))
     (setq SSLWPOLYLINE (ssadd))
     (setq SSCIRCLE (ssadd))
     (setq SSARC (ssadd))
     (setq SSELLIPSE (ssadd))
     (setq SSSPLINE (ssadd))
     (setq SSINSERT (ssadd))
  
  (while (setq EDNA (ssname SUZY COUNTER))
    (setq ED (entget EDNA))
  (cond
   ((= (dxf 0 ED) "LINE")     (ssadd EDNA SSLINE))
   ((= (dxf 0 ED) "LWPOLYLINE") (ssadd EDNA SSLWPOLYLINE))
   ((= (dxf 0 ED) "CIRCLE")     (ssadd EDNA SSCIRCLE))
   ((= (dxf 0 ED) "ARC")    (ssadd EDNA SSARC))
   ((= (dxf 0 ED) "ELLIPSE")    (ssadd EDNA SSELLIPSE))
   ((= (dxf 0 ED) "SPLINE")    (ssadd EDNA SSSPLINE))
   ((= (dxf 0 ED) "INSERT")    (if (not (ASSOC 66 ED))
                        (ssadd EDNA SSINSERT)
                )
   )
   (t nil)
  );END cond
    (setq COUNTER (+ 1 COUNTER))
  );END while

;GET THE FIRST LINE
;COMPARE TO LINES ONLY - IF ED LIST IDENTICAL, DELETE ONE OF THEM
    (setq SSWHICH SSLINE)  (purgeType)
    (setq SSWHICH SSLWPOLYLINE)  (purgeType)
    (setq SSWHICH SSCIRCLE)  (purgeType)
    (setq SSWHICH SSARC)  (purgeType)
    (setq SSWHICH SSELLIPSE)  (purgeType)
    (setq SSWHICH SSSPLINE)  (purgeType)
    (setq SSWHICH SSINSERT)  (purgeType)
)
;----------
(defun dxf (n ed) (cdr (assoc n ed)))
;-----------------
(defun purgeType()
(setq LENGTHSSWHICH (sslength SSWHICH))
;if identical list found, delete the original
 (if (> LENGTHSSWHICH 1)
   (progn
   (setq COUNTER 0)
     
   (while (setq EDNA (ssname  SSWHICH COUNTER))

      (print (dxf 0 ED))
      (setq ED (cddddr (entget EDNA)))
            (setq COUNTERNEXT 1)
            
        (while (setq EDNANEXT (ssname  SSWHICH (+ COUNTER COUNTERNEXT) ))
              (setq NAMY (dxf 0 (entget EDNANEXT)))
                (setq EDNEXT (cddddr (entget EDNANEXT)))
;compares lists EDNA and EDNANEXT, asks if the first list is member of the second list

              (if  (member ED (list EDNEXT))
          (progn
          (print "Duplicate ")(princ NAMY)
          (entdel EDNANEXT)
          (ssdel EDNANEXT SSWHICH)
        );END progn
          (PROGN
            (setq COUNTERNEXT (+ 1 COUNTERNEXT))
            )
          );END if   
            );END while
      ;(print ED)
        (setq COUNTER (+ 1 COUNTER))
    );END while
 ));END if
  (princ)
)