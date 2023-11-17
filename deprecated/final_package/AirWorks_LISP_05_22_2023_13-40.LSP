;THWOMP, THWOMPOLD, QW, DU by Ted Yee 5/19/23, VERTNUM by unknown, AVX, DVX made by Gilles Chanteau 12/05/07
;
;Active layer cannot be a non-training layer, otherwise there is an interrupt error
;
;Commands:
;THWOMP- hides non-training layers, then isolates polylines with <=2nodes
;THWOMPOLD- legacy version of thwomp. adds <=2node polylines to selection set, THWOMPOLD,SELECT,P,ENTER,ENTER,ISOLATE does the same as THWOMP
;QW- same as QSELECT>POLYLINE>CLOSED>NO, hides all except for unclosed training layers
;NB-makes every color By-Layer, displays number of fixes made
;BDT- isolates bad dxf-types (anything that is not a polyline, a circular conc-pad, manhole, or catchbasin, or the latter two as blocks)
;DU- highlights first duplicate it finds
;VERTNUM- selecting polyline prints number of vertices (sometimes off by 1)
;AVX- Adds Vertex on Pline (LW, 2D, 3D) 
;DVX- Deletes Vertex on Pline (LW, 2D, 3D)

(defun c:thwomp ()
   (command "_layer" "_off" "R-PAVEMARK-LINE" "")
   (command "_layer" "_off" "GEOM-CHECKER-ARCHIVE" "")
   (command "_layer" "_off" "U-MANHOLE" "")
   (command "_layer" "_off" "U-CATCHBASIN" "")
   (command "_layer" "_off" "A-OBSTRUCTION" "")
   (command "_layer" "_off" "G-TOPO-MINR" "")
   (command "_layer" "_off" "G-TOPO-MAJR" "")
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
   (command "PSELECT" "P" "")   
   (command "ISOLATEOBJECTS"))


(defun c:thwompold ()
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))


(defun c:qw ()
   (command "_layer" "_off" "R-PAVEMARK-LINE" "")
   (command "_layer" "_off" "GEOM-CHECKER-ARCHIVE" "")
   (command "_layer" "_off" "U-MANHOLE" "")
   (command "_layer" "_off" "U-CATCHBASIN" "")
   (ssget "X" '((0 . "LWPOLYLINE") (70 . 0)))
   (command "Pselect" "P" "")   
   (command "ISOLATEOBJECTS"))


(defun c:nb
 (print "Number of Non-By-Layer objects fixed:")
 (command "_SetByLayer" allobjects "" "Yes" "Yes"))


(defun C:bdt ()
  (command "_layer" "_off" "G-TOPO-MINR" "")
  (command "_layer" "_off" "G-TOPO-MAJR" "")
  
  (setq A (ssget "X" '((0 . "LWPOLYLINE"))))                ;"A","B","C" are lists of acceptable entities
  (setq B (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))
  (setq C (ssget "X" '((0 . "INSERT") (8 . "U-MANHOLE,U-CATCHBASIN"))))
  
  ;Appender Loops
  (setq n (sslength B))                                     ;"n"=number of B
  (setq i -1)                                               ;"i"=-1 (0 at beginning of list)
  (repeat n (ssadd (ssname B (setq i (1+ i))) A))           ;appends ID of element #i in list B to list A
  
  (setq o (sslength C))                                   
  (setq j -1)                                             
  (repeat o (ssadd (ssname C (setq j (1+ j))) A))         

  (command "PSELECT" A "")                                  ;isolates list A, which now includes B,C elements too
  (command "HIDEOBJECTS")
)


(defun C:du ()
  (setq SUZY (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,INSERT")))) ;makes "SUZY" a set of entities of these types
  (setq COUNTER 0)
  (setq duplicatelist (ssadd))
  (setq numdups (sslength duplicatelist))
  
  (setq SSLINE        (ssadd))                              ;creating new selection sets for each entity type
  (setq SSLWPOLYLINE  (ssadd))
  (setq SSCIRCLE      (ssadd))
  (setq SSARC         (ssadd))
  (setq SSELLIPSE     (ssadd))
  (setq SSSPLINE      (ssadd))
  (setq SSINSERT      (ssadd))                              ;INSERT is for blocks
  
  (while 
    (setq EDNA (ssname SUZY COUNTER))                       ;sets value of EDNA to the value of SUZY at the index location of counter
    (setq ED (entget EDNA))                                 ;sets value od ED to the entity info of EDNA
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
  
  (setq entityType SSLINE)        (findit)                  ;changes the search area from one entity type to the next
  (setq entityType SSLWPOLYLINE)  (findit)
  (setq entityType SSCIRCLE)      (findit)
  (setq entityType SSARC)         (findit)
  (setq entityType SSELLIPSE)     (findit)
  (setq entityType SSSPLINE)      (findit)
  (setq entityType SSINSERT)      (findit)
  
  (print "Number of Total Duplicates:")                     ;The end of the code
  (command "PSELECT" DUPLICATELIST "")   
  (command "ISOLATEOBJECTS")
)
(defun dxf (n ed) (cdr (assoc n ed)))                       ;subfunction "dxf" needs inputs "n" and "ED", takes all but 1st info searhes nth element in list "ED"
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
            (progn
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


(defun c:vertnum ()
   (vl-load-com)
   (setq Pl_Ent (car (entsel "\nSelect polyline: ")))
   (setq VL_Obj (vlax-ename->vla-object Pl_Ent))
   (setq endParam (vlax-curve-getEndParam Vl_Obj))
   (setq NumVert (+ 0 endParam))
   (princ (strcat "\n" (rtos NumVert 2 0) " vertices"))
   (princ))