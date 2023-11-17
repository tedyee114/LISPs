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


(defun c:nb ()
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



(defun c:avx	(/    err  AcDoc     pl	  ob   pk   pa	 ap   typ  org
		 ucs  ocs  pt	sp   ep	  co   no   p1	 p2   pt   ce
		 a1   a2   bu
		)

  (vl-load-com)

  (defun err (msg)
    (if	(or
	  (= msg "Fonction annul�e")
	  (= msg "quitter / sortir abandon")
	)
      (princ)
      (princ (strcat "\nErreur: " msg))
    )
    (and ucs (vla-put-activeUCS AcDoc ucs))
    (and ocs (vla-delete ocs) (setq ocs nil))
    (vla-EndUndoMark
      (vla-get-ActiveDocument (vlax-get-acad-object))
    )
    (setq *error* m:err
	  m:err	nil
    )
  )

  (setq	m:err	*error*
	*error*	err
	AcDoc	(vla-get-activeDocument (vlax-get-acad-object))
  )
  (while (and
	   (setq pl (entsel))
	   (setq ob (vlax-ename->vla-object (car pl)))
	   (setq typ (vla-get-Objectname ob))
	 )
    (if	(or (= typ "AcDbPolyline")
	    (and (member typ '("AcDb2dPolyline" "AcDb3dPolyline"))
		 (= 0 (vla-get-Type ob))
	    )
	)
      (progn
	(vla-StartUndoMark AcDoc)
	(setq pk
	       (if (= typ "AcDb3dPolyline")
		 (trans (osnap (cadr pl) "_nea") 1 0)
		 (vlax-curve-getClosestPointToProjection
		   ob
		   (trans (cadr pl) 1 0)
		   (mapcar '-
			   (trans (getvar "VIEWDIR") 1 0)
			   (trans '(0 0 0) 1 0)
		   )
		 )
	       )
	)
	(setq ap (/ (* (getvar "APERTURE")
		       (getvar "VIEWSIZE")
		    )
		    (cadr (getvar "SCREENSIZE"))
		 )
	)
	(if (= typ "AcDbPolyline")
	  (setq co (split-list (vlax-get ob 'Coordinates) 2))
	  (setq co (split-list (vlax-get ob 'Coordinates) 3))
	)
	(cond
	  ((equal pk (vlax-curve-getStartPoint ob) ap)
	   (setq pa 0)
	   (if (= (vla-get-Closed ob) :vlax-false)
	     (setq sp (vlax-curve-getStartPoint ob)
		   ep nil
	     )
	     (setq ep nil
		   sp nil
	     )
	   )
	  )
	  ((equal pk (vlax-curve-getEndPoint ob) ap)
	   (setq pa (1- (length co)))
	   (if (= (vla-get-Closed ob) :vlax-false)
	     (setq ep (vlax-curve-getEndPoint ob)
		   sp nil
	     )
	     (setq ep nil
		   sp nil
	     )
	   )
	  )
	  (T
	   (setq pa (atoi (rtos (vlax-curve-getParamAtPoint ob pk)))
		 ep nil
		 sp nil
	   )
	  )
	)
	(if (and (/= typ "AcDb3dPolyline")
		 (or
		   (not	(equal (trans '(0 0 1) 1 0 T)
			       (setq no (vlax-get ob 'Normal))
			       1e-9
			)
		   )
		   (and	(= typ "AcDbPolyline")
			(/= 0 (vla-get-Elevation ob))
		   )
		   (and (= typ "AcDb2dPolyline") (/= 0 (caddar co)))
		 )
	    )
	  (progn
	    (setq ucs (vla-add
			(vla-get-UserCoordinateSystems AcDoc)
			(vlax-3d-point (setq org (getvar "UCSORG")))
			(vlax-3d-point (mapcar '+ org (getvar "UCSXDIR")))
			(vlax-3d-point (mapcar '+ org (getvar "UCSYDIR")))
			"avxUCS"
		      )
		  ocs (vla-add
			(vla-get-UserCoordinateSystems AcDoc)
			(vlax-3d-Point
			  (setq org (vlax-curve-getStartPoint ob))
			)
			(vlax-3d-Point
			  (mapcar '+ org (trans '(1 0 0) no 0))
			)
			(vlax-3d-Point
			  (mapcar '+ org (trans '(0 1 0) no 0))
			)
			"avxOCS"
		      )
	    )
	    (vla-put-activeUCS AcDoc ocs)
	  )
	)
	(if (setq
	      pt
	       (getpoint (trans (vlax-curve-getPointAtParam ob pa) 0 1) 

;;;			 "\nSpecifiez le sommet � ajouter: " 
			 "\nSpecify the Segment/Arc for Vertex to Add : " 

	       )
	    )
	  (progn
	    (and ep (setq pa (- (length co) 2)))
	    (if	(/= typ "AcDb3dPolyline")
	      (progn
		(setq p1 (trans (vlax-curve-getPointAtParam ob pa) 0 no)
		      pt (trans pt 1 no)
		      p2 (trans	(vlax-curve-getPointAtParam ob (1+ pa))
				0
				no
			 )
		)
		(cond
		  ((and ep (/= 0 (vla-getBulge ob pa)))
		   ((lambda (a)
		      (setq
			bu
			 (list (cons (1+ (fix pa)) (/ (sin a) (cos a))))
		      )
		    )
		     (/
		       (- (angle p2 pt)
			  (+ (angle p2 p1)
			     (* 2 (atan (vla-getBulge ob pa)))
			     pi
			  )
		       )
		       2.0
		     )
		   )
		  )
		  ((and sp (/= 0 (vla-getBulge ob pa)))
		   ((lambda (a)
		      (setq
			bu (list (cons 0 (/ (sin a) (cos a))))
		      )
		    )
		     (/
		       (- (+ (angle p1 p2)
			     (* -2 (atan (vla-getBulge ob pa)))
			     pi
			  )
			  (angle p1 pt)
		       )
		       2.0
		     )
		   )
		  )
		  (T
		   (setq
		     ce	((lambda (mid1 mid2)
			   (inters mid1
				   (polar mid1
					  (+ (angle p1 pt) (/ pi 2))
					  1.0
				   )
				   mid2
				   (polar mid2
					  (+ (angle pt p2) (/ pi 2))
					  1.0
				   )
				   nil
			   )
			 )
			  (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0))
				  p1
				  pt
			  )
			  (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0))
				  pt
				  p2
			  )
			)
		   )
		   (if (or (= 0 (vla-getBulge ob pa)) (null ce))
		     (setq a1 0.0
			   a2 0.0
		     )
		     (if (< pi
			    (ang<2pi (- (angle pt p2) (angle p1 pt)))
			    (* 2 pi)
			 )
		       (setq a1	(- (ang<2pi (- (angle ce p1) (angle ce pt)))
				)
			     a2	(- (ang<2pi (- (angle ce pt) (angle ce p2)))
				)
		       )
		       (setq a1	(ang<2pi (- (angle ce pt) (angle ce p1)))
			     a2	(ang<2pi (- (angle ce p2) (angle ce pt)))
		       )
		     )
		   )
		   (setq bu
			  (list	(cons pa (/ (sin (/ a1 4.0)) (cos (/ a1 4.0))))
				(cons (1+ (fix pa))
				      (/ (sin (/ a2 4.0)) (cos (/ a2 4.0)))
				)
			  )
		   )
		  )
		)
	      )
	    )
	    (cond
	      ((= typ "AcDbPolyline")
	       (setq pt (list (car pt) (cadr pt)))
	      )
	      ((= typ "AcDb3dPolyline") (setq pt (trans pt 1 0)))
	    )
	    (or sp (setq pa (1+ pa)))
	    (cond
	      (sp (setq co (cons pt co)))
	      (ep (setq co (append co (list pt))))
	      (T
	       (setq co	(append	(sublst co 1 pa)
				(cons pt (sublst co (1+ pa) nil))
			)
	       )
	      )
	    )
	    (or
	      (= typ "AcDb3dPolyline")
	      (while (<= (setq pa (1+ pa)) (vlax-curve-getEndParam ob))
		(setq bu (cons (cons pa (vla-getBulge ob (1- pa))) bu))
	      )
	    )
	    (vlax-put ob 'Coordinates (apply 'append co))
	    (or	(= typ "AcDb3dPolyline")
		(mapcar	'(lambda (x) (vla-setBulge ob (car x) (cdr x)))
			bu
		)
	    )
	    (and ucs (vla-put-activeUCS AcDoc ucs))
	    (vla-EndUndoMark AcDoc)
	  )
	)
      )
      (progn 

;;	(alert "Entit� non valide.") 
	(alert "Entity Not Valid.") 

	(exit)
      )
    )
  )
  (and ocs (vla-delete ocs) (setq ocs nil))
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)



(defun c:dvx	(/ err os pt ent typ plst par blst n)
  (vl-load-com)

  (defun err (msg)
    (if	(or
	  (= msg "Fonction annul�e") 
	  (= msg "quitter / sortir abandon") 
	)
      (princ) 

      (princ (strcat "\nErreur: " msg))
    )
    (vla-EndUndoMark
      (vla-get-ActiveDocument (vlax-get-acad-object))
    )
    (setvar "OSMODE" os)
    (setq *error* m:err
	  m:err	nil
    )
  )

  (setq	m:err	*error*
	*error*	err
	os	(getvar "OSMODE")
  )
  (setvar "OSMODE" 1)
  (while (setq pt
		(getpoint 

;;		  "\nS�lectionnez le sommet � supprimer: " 
		  "\nSelect the Vertex to Del/Erase: " 

		)
	 )
    (if	(and
	  (setq	ent (ssget pt
			   '((-4 . "<OR")
			     (0 . "LWPOLYLINE")
			     (-4 . "<AND")
			     (0 . "POLYLINE")
			     (-4 . "<NOT")
			     (-4 . "&")
			     (70 . 118)
			     (-4 . "NOT>")
			     (-4 . "AND>")
			     (-4 . "OR>")
			    )
		    )
	  )
	  (setq ent (vlax-ename->vla-object (ssname ent 0)))
	  (setq typ (vla-get-ObjectName ent))
	)
      (if
	(and
	  (setq	plst (if (= typ "AcDbPolyline")
		       (split-list (vlax-get ent 'Coordinates) 2)
		       (split-list (vlax-get ent 'Coordinates) 3)
		     )
	  )
	  (< 2 (length plst))
	)
	 (progn
	   (vla-StartUndoMark
	     (vla-get-ActiveDocument (vlax-get-acad-object))
	   )
	   (setq pt   (trans pt 1 0)
		 par  (cond
			((equal pt (vlax-curve-getStartPoint ent) 1e-9)
			 0
			)
			((equal pt (vlax-curve-getEndPoint ent) 1e-9)
			 (1- (length plst))
			)
			(T
			 (atoi (rtos (vlax-curve-getParamAtPoint ent pt))
			 )
			)
		      )
		 blst nil
		 n    0
	   )
	   (or (= typ "AcDb3dPolyline")
	       (repeat (length plst)
		 (if (/= n par)
		   (setq
		     blst
		      (cons (cons (length blst) (vla-getBulge ent n))
			    blst
		      )
		   )
		 )
		 (setq n (1+ n))
	       )
	   )
	   (vlax-put ent
		     'Coordinates
		     (apply 'append (vl-remove (nth par plst) plst))
	   )
	   (or (= typ "AcDb3dPolyline")
	       (mapcar '(lambda (x) (vla-setBulge ent (car x) (cdr x)))
		       blst
	       )
	   )
	   (vla-EndUndoMark
	     (vla-get-ActiveDocument (vlax-get-acad-object))
	   )
	 )
	(progn 

;; 	  (alert "\nLa polyligne n'a que deux sommets. ") 
	  (alert "\nThe Pline has ONLY 2 Vertex. ") 

	  (exit)
	)
      )
      (progn 

;;	(alert "Entit� non valide.") 
	(alert "Entity not Valid.") 

	(exit)
      )
    )
  )
  (setvar "OSMODE" os)
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)

;;; SUBLST Retourne une sous-liste
;;; Premier �l�ment : 1
;;; (sublst '(1 2 3 4 5 6) 3 2) -> (3 4)
;;; (sublst '(1 2 3 4 5 6) 3 nil) -> (3 4 5 6)

(defun sublst (lst start leng / rslt)
  (if (not (<= 1 leng (- (length lst) start)))
    (setq leng (- (length lst) (1- start)))
  )
  (repeat leng
    (setq rslt	(cons (nth (1- start) lst) rslt)
	  start	(1+ start)
    )
  )
  (reverse rslt)
)

;; SPLIT-LIST Retourne une liste de sous-listes
;; Arguments
;; - lst : la lste � fractionner
;; - num : un entier, le nombre d'�l�ments des sous listes
;; Exemples :
;; (split-list '(1 2 3 4 5 6 7 8) 2) -> ((1 2) (3 4) (5 6) (7 8))
;; (split-list '(1 2 3 4 5 6 7 8) 3) -> ((1 2 3) (4 5 6) (7 8))

(defun split-list (lst n)
  (if lst
    (cons (sublst lst 1 n)
	  (split-list (sublst lst (1+ n) nil) n)
    )
  )
)

;;; Ang<2pi Retourne l'angle, � 2*k*pi pr�s, compris entre 0 et 2*pi

(defun ang<2pi (ang)
  (if (and (<= 0 ang) (< ang (* 2 pi)))
    ang
    (ang<2pi (rem (+ ang (* 2 pi)) (* 2 pi)))
  )
)