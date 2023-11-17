;THWOMP, THWOMPOLD, QW, NB, BDT, DU by Ted Yee 5/22/23, VERTNUM by unknown, SAYIT by Terry CADD 01/18/2007, AVX, DVX made by Gilles Chanteau 12/05/07
;
;Active layer cannot be a non-training layer, otherwise there is an interrupt error
;
;Commands:
;SAYIT- reads out loud whatever words are typed, for fun, lol.
;THWOMP- hides non-training layers, then isolates polylines with <=2nodes
;QW- same as QSELECT>POLYLINE>CLOSED>NO, hides all except for unclosed training layers
;NB-makes every color By-Layer, displays number of fixes made
;GER- same as MAPCLEAN>INTERACTIVE>BREAK CROSSING OBJECTS> MARK ALL, locates geos-r objects, requires gera.dpf file
;BDT- isolates bad dxf-types (anything that is not a polyline, a circular conc-pad, manhole, or catchbasin, or the latter two as blocks)
;DU- highlights first duplicate it finds
;VERTNUM- selecting polyline prints number of vertices (sometimes off by 1)
;AVX- Adds Vertex on Pline (LW, 2D, 3D) 
;DVX- Deletes Vertex on Pline (LW, 2D, 3D)

(defun c:SayIt (/ Phrase Sapi)
  (setq Phrase (getstring "\nEnter phrase: " t))
  (setq Sapi (vlax-create-object "Sapi.SpVoice"))
  (vlax-invoke Sapi "Speak" Phrase 0)
  (vlax-release-object Sapi)
  (princ))

(defun c:vertnum ()  ;prints number of vertices---------------------------------------------------------------------------------------------------------------------------------------------
   (vl-load-com)
   (setq Pl_Ent (car (entsel "\nSelect polyline: ")))
   (setq VL_Obj (vlax-ename->vla-object Pl_Ent))
   (setq endParam (vlax-curve-getEndParam Vl_Obj))
   (setq NumVert (+ 0 endParam))
   (princ (strcat "\n" (rtos NumVert 2 0) " vertices"))
   (princ))

(defun c:thwomp ()  ;isolates 2node polys---------------------------------------------------------------------------------------------------------------------------------------------
  (command "LAYER" "FREEZE" "R-PAVEMARK-LINE" "")                        ;freezes non-training layers
  (command "LAYER" "FREEZE" "GEOM-CHECKER-ARCHIVE" "")
  (command "LAYER" "FREEZE" "U-MANHOLE" "")
  (command "LAYER" "FREEZE" "U-CATCHBASIN" "")
  (command "LAYER" "FREEZE" "A-OBSTRUCTION" "")
  (command "LAYER" "FREEZE" "Contour_Line_Intermediate" "")
  (command "LAYER" "FREEZE" "Contour_Line_Major" "")
  (command "LAYER" "FREEZE" "G-TOPO-MINR" "")
  (command "LAYER" "FREEZE" "G-TOPO-MAJR" "")
  (command "LAYER" "FREEZE" "0" "")
  (ssget "A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))                 ;"A"=search All unfrozen layers, attribute type 0 is layername, -4 allows operators, attribute 90 is #vertices
  (command "LAYTHW")                                                    ;thaws (unfreezes) all layers
  (command "ISOLATEOBJECTS" "P" "" ""))                                  ;isolates previous (most recent) entity selection set


(defun c:qw () ;isolates unclosed polylines---------------------------------------------------------------------------------------------------------------------------------------------
  (command "LAYER" "FREEZE" "R-PAVEMARK-LINE" "")                        ;freezes non-training layers
  (command "LAYER" "FREEZE" "GEOM-CHECKER-ARCHIVE" "")
  (command "LAYER" "FREEZE" "U-MANHOLE" "")
  (command "LAYER" "FREEZE" "U-CATCHBASIN" "")
  (command "LAYER" "FREEZE" "A-OBSTRUCTION" "")
  (command "LAYER" "FREEZE" "Contour_Line_Intermediate" "")
  (command "LAYER" "FREEZE" "Contour_Line_Major" "")
  (command "LAYER" "FREEZE" "G-TOPO-MINR" "")
  (command "LAYER" "FREEZE" "G-TOPO-MAJR" "")
  (command "LAYER" "FREEZE" "0" "")
  (ssget "A" '((0 . "LWPOLYLINE") (70 . 0)))                             ;"A"=search All unfrozen layers, attribute type 70 is closed, 0=false
  (command "LAYTHW")                                                    ;thaws (unfreezes) all layers
  (command "ISOLATEOBJECTS" "P" "" ""))                                  ;isolates previous (most recent) entity selection set
  

(defun c:nb () ;makes every color By-Layer, displays number of fixes made---------------------------------------------------------------------------------------------------------------------------------------------
 (print "Number of Non-By-Layer objects fixed:")
 (command "SetByLayer" "ALL" "" "Yes" "Yes"))

(defun c:ger ()
  (print "Please isolate the layer you want to search for GEOS-R errors")
  (command "MAPCLEAN" "gera")                                           ;runs saved cleanup profile called gera.dpf, which specifies putting markers on intersections
)


(defun C:bdt () ;isolates bad dxf types---------------------------------------------------------------------------------------------------------------------------------------------
  (command "LAYER" "FREEZE" "R-PAVEMARK-LINE" "")                        ;freezes non-training layers
  (command "LAYER" "FREEZE" "GEOM-CHECKER-ARCHIVE" "")
  (command "LAYER" "FREEZE" "U-MANHOLE" "")
  (command "LAYER" "FREEZE" "U-CATCHBASIN" "")
  (command "LAYER" "FREEZE" "A-OBSTRUCTION" "")
  (command "LAYER" "FREEZE" "Contour_Line_Intermediate" "")
  (command "LAYER" "FREEZE" "Contour_Line_Major" "")
  (command "LAYER" "FREEZE" "G-TOPO-MINR" "")
  (command "LAYER" "FREEZE" "G-TOPO-MAJR" "")
  (command "LAYER" "FREEZE" "0" "")
  
  (setq A (ssget "A"))                                                 ;"A"=entity selection set of all unfrozen ("A") entities
  (setq B (ssget "A" '((0 . "LWPOLYLINE"))))                           ;"B","C" are entity selection sets of acceptable entities
  (setq C (ssget "A" '((0 . "CIRCLE"))))
  (setq D (ssget "A" '((0 . "LWPOLYLINE") (8 . "U-MANHOLE-TR"))))      ;"D"=polyline manholes, unacceptable entities
  (command "LAYTHW")                                                  ;thaws (unfreezes) all layers


          ;these loops remove entity selection sets "B" and "C" from "A", then add "D" back to it
  (if (/= B nil)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
  (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
    (setq n (sslength B))                                              ;"n"=number of B
    (setq i -1)                                                        ;"i"=-1 (0 at beginning of list)
    (repeat n (ssdel (ssname B (setq i (1+ i))) A))))                  ;removes ID of i'th element in list "B" from list "A", repeats until it has worked n number of times

  (if (/= C nil)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
  (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
    (setq m (sslength C))                                              ;"m"=number of C
    (setq j -1)                                                        ;"j"=-1 (0 at beginning of list)
    (repeat m (ssdel (ssname C (setq j (1+ j))) A))))                  ;removes ID of i'th element in list "C" from list "A", repeats until it has worked m number of times

  (if (/= D nil)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
  (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
    (setq p (sslength D))                                              ;"m"=number of C
    (setq q -1)                                                        ;"j"=-1 (0 at beginning of list)
    (repeat p (ssadd (ssname D (setq q (1+ q))) A))))                  ;removes ID of i'th element in list "C" from list "A", repeats until it has worked p number of times         

  (command "PSELECT" A "" "ISOLATEOBJECTS")                            ;selects, then isolates entity selection set "A" which has now been altered since creation
)


(defun C:du ()   ;isolates duplicate entities---------------------------------------------------------------------------------------------------------------------------------------------
  (setq SUZY (ssget "A" '((0 . "LWPOLYLINE,CIRCLE"))))                 ;"SUZY"=entity selection set containing all ("A") unfrozen LWPOLYLINES and CIRCLES
  (setq COUNTER 0)                                                     ;"COUNTER"=0, used to iterate through every object below
  (setq DU (ssadd))                                                    ;creates "DU", the entity selection set that holds one of every duplicate entity
  (setq LWPOLYLINELIST  (ssadd))                                       ;creates empty entity selection set
  (setq CIRCLELIST      (ssadd))                                       ;creates empty entity selection set
  
  (while                  ;this block adds the entities to the selection sets
    (setq EDNA (ssname SUZY COUNTER))                                  ;"EDNA"= value COUNTER'th element in list"SUZY"
    (setq ED (entget EDNA))                                            ;"ED"=attributes of entity EDNA
    (cond                                                              ;condition of while function
      ((= (cdr (assoc 0 ED)) "LWPOLYLINE") (ssadd EDNA LWPOLYLINELIST)) ;if layername found within of "ED" (attribute list of EDNA) is "LWPOLYLINE", add EDNA to the list "LWPOLYLINELIST"
      ((= (cdr (assoc 0 ED)) "CIRCLE")     (ssadd EDNA CIRCLELIST)))    ;if layername found within of "ED" (attribute list of EDNA) is "LWPOLYLINE", add EDNA to the list "CIRCLELIST"
    (setq COUNTER (1+ COUNTER))                                        ;increments counter up one
  )
  
  (setq entityType LWPOLYLINELIST)  (findit)                           ;changes the search area from one entity type to the next
  (setq entityType CIRCLELIST)      (findit)

  (print "Number of Total Duplicates:") (princ (sslength DU))          ;the end of the code, pretty self-explanatory
  (command "PSELECT" DU "" "ISOLATEOBJECTS")                           ;selects, then isolates entity selection set "A" which has now been altered since creation
)

(defun c:swag ()
  (princ "Working. This should take a few seconds...THWOMP...THWOMP...THWOMP...")
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
    (command "LAYER" "FREEZE" "0" "")
    (setq UC (ssget "A" '((0 . "LWPOLYLINE") (70 . 0))))                 ;"UC"=entity selection set of all unclosed polylines on unfrozen ("A") layers
  
  ;duplicates - creates entity selection set DU-------------------------------------------------------------------------------------------------------------------------
    (setq SUZY (ssget "A" '((0 . "LWPOLYLINE,CIRCLE"))))                 ;"SUZY"=entity selection set containing all ("A") unfrozen LWPOLYLINES and CIRCLES
    (setq COUNTER 0)                                                     ;"COUNTER"=0, used to iterate through every object below
    (setq DU (ssadd))                                                    ;creates "DU", the entity selection set that holds one of every duplicate entity
    (setq LWPOLYLINELIST  (ssadd))                                       ;creates empty entity selection set
    (setq CIRCLELIST      (ssadd))                                       ;creates empty entity selection set
    
    (while                  ;this block adds the entities to the selection sets
      (setq EDNA (ssname SUZY COUNTER))                                  ;"EDNA"= value COUNTER'th element in list"SUZY"
      (setq ED (entget EDNA))                                            ;"ED"=attributes of entity EDNA
      (cond                                                              ;condition of while function
       ((= (cdr (assoc 0 ED)) "LWPOLYLINE") (ssadd EDNA LWPOLYLINELIST)) ;if layername found within of "ED" (attribute list of EDNA) is "LWPOLYLINE", add EDNA to the list "LWPOLYLINELIST"
       ((= (cdr (assoc 0 ED)) "CIRCLE")     (ssadd EDNA CIRCLELIST)))    ;if layername found within of "ED" (attribute list of EDNA) is "LWPOLYLINE", add EDNA to the list "CIRCLELIST"
      (setq COUNTER (1+ COUNTER))                                        ;increments counter up one
    )
    
    (setq entityType LWPOLYLINELIST)  (findit)                           ;changes the search area from one entity type to the next
    (setq entityType CIRCLELIST)      (findit)
  
  ;bad-dxf-entity-types - creates entity selection set A----------------------------------------------------------------------------------
    (setq A (ssget "A"))                                                 ;"A"=entity selection set of all unfrozen ("A") entities
    (setq B (ssget "A" '((0 . "LWPOLYLINE"))))                           ;"B","C" are entity selection sets of acceptable entities
    (setq C (ssget "A" '((0 . "CIRCLE"))))
    (setq D (ssget "A" '((0 . "LWPOLYLINE") (8 . "U-MANHOLE-TR"))))      ;"D"=polyline manholes, unacceptable entities

           ;these loops remove entity selection sets "B" and "C" from "A", then add "D" back to it
    (if (/= B nil)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
    (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
      (setq n (sslength B))                                              ;"n"=number of B
      (setq i -1)                                                        ;"i"=-1 (0 at beginning of list)
      (repeat n (ssdel (ssname B (setq i (1+ i))) A))))                  ;removes ID of i'th element in list "B" from list "A", repeats until it has worked n number of times

    (if (/= C nil)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
    (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
      (setq m (sslength C))                                              ;"m"=number of C
      (setq j -1)                                                        ;"j"=-1 (0 at beginning of list)
      (repeat m (ssdel (ssname C (setq j (1+ j))) A))))                  ;removes ID of i'th element in list "C" from list "A", repeats until it has worked m number of times

    (if (/= D nil)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
    (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
      (setq p (sslength D))                                              ;"m"=number of C
      (setq q -1)                                                        ;"j"=-1 (0 at beginning of list)
      (repeat p (ssadd (ssname D (setq q (1+ q))) A))))                  ;removes ID of i'th element in list "C" from list "A", repeats until it has worked p number of times

  ;2nodes - creates entity selection set TN--------------------------------------------------------------------------------------------------
    (setq TN (ssget "A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))     ;"TN"= entity selection set of all unfrozen ("A") polylines
    (command "_LAYTHW")                                                  ;thaws (unfreezes) all layers
  
  ;GEOS-R-errors - creates list (not selection set) EL---------------------------------------------------------------------------------------
    (setq EL (list ": "))                                                ;makes a list that will hold names of layers with errors
    (setq LAYCUR "B-OVERHANG")          (cleaner)                        ;sets current search layer, then runs subfunction called "cleaner"
    (setq LAYCUR "C-CONC-PAD")          (cleaner)
    (setq LAYCUR "R-CURB")              (cleaner)
    (setq LAYCUR "R-CURB-BERM")         (cleaner)
    (setq LAYCUR "R-GUTTER")            (cleaner)
    (setq LAYCUR "R-PAVEMARK-LINE-TR")  (cleaner)
    (setq LAYCUR "R-PAVEMARK-POLY")     (cleaner)
    (setq LAYCUR "R-PAVEMARK-SYMBOL")   (cleaner)
    (setq LAYCUR "R-RAILROAD-TR")       (cleaner)
    (setq LAYCUR "R-ROAD-ASPH")         (cleaner)
    (setq LAYCUR "R-ROAD-CONC")         (cleaner)
    (setq LAYCUR "R-ROAD-PAVER")        (cleaner)
    (setq LAYCUR "R-WALK-ASPH")         (cleaner)
    (setq LAYCUR "R-WALK-CONC")         (cleaner)
    (setq LAYCUR "R-WALK-PAVER")        (cleaner)
    (setq LAYCUR "U-CATCHBASIN-TR")     (cleaner)
    (setq LAYCUR "U-MANHOLE-TR")        (cleaner)
    (setq LAYCUR "U-SOLAR")             (cleaner)
    (setq LAYCUR "V-VEGETATION")        (cleaner)
    (setq LAYCUR "W-WATER")             (cleaner)
  
  ;printout--------------------------------------------------------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")                         ;sets all layer colors to "By-Layer"
    (print "Non-By-Layers Fixed per Output Above ^^")
    (print "Unclosed Polylines:   ") (printsetelements UC)  
    (print "Duplicates:           ") (printsetelements DU)
    (print "Bad-DXF-Entity-Types: ") (printsetelements A)
    (print "2-Node Polys:         ") (printsetelements TN)
    (print "GEOS-R Errors:        ") (princ (length EL)) (princ EL)
    (setq Sapi (vlax-create-object "Sapi.SpVoice")) (vlax-invoke Sapi "Speak" "thwomp" 0) (vlax-release-object Sapi) ;plays sound when command is done
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
            (progn                                                       ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
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


(defun printsetelements (inputlist)
  (if (/= inputlist nil)                                                 ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
  (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
    (princ (setq o (sslength inputlist)))(princ ":")                     ;sets "o" to the length of "inputlist" and then prints it
    (setq k -1)                                                          ;"k"=-1 (0 at beginning of list)
    (repeat o
      (setq CURRENT (ssname inputlist (setq k (1+ k))))                  ;updates k, then sets "CURRENT"=ID of k'th entity in list "inputlist"
      (princ " ")(princ (+ 1 k))(princ ".")                              ;formatting shmuck so that list gets printed as "1. B-OVERHANG, 2. R-CURB, etc"
      (princ (cdr (assoc 8 (entget CURRENT))))                           ;gets layername of CURRENT- searches within the entity info for value type 8 (layer)
    )
  ))
)


(defun cleaner ()
  (if (tblsearch "layer" LAYCUR)                                         ;if current layer doesn't exist, skip all of the subfunction "cleaner"
  (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
    (if (not (tblsearch "layer" "GEOS-R-TEMP"))                          ;if dummy layer doesn't exist, create it
      (command "_layer" "m" "GEOS-R-TEMP" ""))
    
    (command "_layer" "set" LAYCUR "")                                   ;this block turns off everything except current layer and dummy layer
    (command "_layer" "_off" "*" "N" "")
    (command "_layer" "_on" LAYCUR "_on" "GEOS-R-TEMP" "")

    (command "_mapclean" "GEOS-R-TEMP")                                  ;if mapclean finds errors, they are placed on dummy layer
    
    (setq newobjects (ssget "X" '((8 . "GEOS-R-TEMP"))))                 ;"NEWOBJECTS"=entity selection set of everything on dummy layer
    (if (/= newobjects nil)                                              ;if that set is not nil (empty), mark current layer as errored
      (setq EL (append EL (list LAYCUR))))
    
    (command "_laydel" "n" "GEOS-R-TEMP" "" "Y" "")                      ;delete dummy layer and turn all layers back on
    (command "_layer" "_on" "*" "")
  )
  )
)




;add and delete vertices from polylines---------------------------------------------------------------------------------------------------------------------------------------------
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