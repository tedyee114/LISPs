;THWOMP, UC, NB, GER, BDT, DU, SWAG by Ted Yee 9/18/23, VERTNUM by unknown, SAYIT by Terry CADD 01/18/2007, BUFF by Kent1Cooper Autodesk Forums, adapted by Ted Yee 9/15/2023, AVX, DVX  by Gilles Chanteau 12/05/07
;
;Active layer cannot be a non-training layer, otherwise there is an interrupt error
;There were many versions of this, comments may not have been updated. If comment looks verywrong, it probably is outdated
;
;User Commands:
;SAYIT-   LINE 033- reads out loud inputted text. For fun, lol
;VERTNUM- LINE 043- prints number of vertices of selected polyline (sometimes off by 1)
;BUFF-    LINE 055- breates buffer around polylines with entered values being full cross-width, options for endcaps and smooth corners
;THWOMP-  LINE 129- hides company non-training layers, then isolates polylines with <=2nodes
;UC-      LINE 148- same as QSELECT>POLYLINE>CLOSED>NO, isolates unclosed polylines on training layers
;NB-      LINE 166- makes every entity's color "By-Layer", displays number of fixes made
;GER-     LINE 173- *requires GER.dpf* runs MAPCLEAN with GER.dpf= MAPCLEAN>INTERACTIVE>BREAK CROSSING OBJECTS>FINISH (these saved as GER.pf), locates geos-r objects
;BDT-     LINE 180- isolates "bad dxf-types" (unacceptable entities at company: anything that is not a polyline, a circle, or circular U-MANHOLE-TR)
;DU-      LINE 224- *very calculation heavy* isolates duplicate objects
;SWAG-    LINE 208- *requires GEOS-R-TEMP.dpf & DUPL.dpf* file checker like company used to use on swagger.io, checks for 2node polys, uncloseds, bad-dxf-types, geos-r errors, and fixes non-by-layer-colored and duplicate entities.
;AVX-     LINE 467- Adds Vertex on polyline (LW, 2D, 3D) 
;DVX-     LINE 761- Deletes Vertex on polyline (LW, 2D, 3D)

;;Optional manual input of drawing cleanup file locations: (must use double slash \\, otherwise invalid)
;(setq GER_LOCATION         "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\GER.dpf")          ;GER.dpf is same as MAPCLEAN>BREAK CROSSING OBJECTS>INTERACTIVE
;(setq DUPL_LOCATION        "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\DUPL.dpf")         ;DUPL.dpf is same as MAPCLEAN>DELETE DUPLICATES>AUTOMATIC
;(setq GEOS-R-TEMP_LOCATION "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\GEOS-R-TEMP.dpf")  ;GEOS-R-TEMP.dpf is same as MAPCLEAN>BREAK CROSSING OBJECTS>AUTOMATIC, RETAIN ORIGINALS AND CREATE NEW OBJECTS ON LAYER "GEOS-R-TEMP"

;Drawing Cleanup Profile (.dpf) locations, required for GER and SWAG
(setq GER_LOCATION         (findfile "GER.dpf"))                                                      ;GER.dpf is same as MAPCLEAN>BREAK CROSSING OBJECTS>INTERACTIVE
(setq DUPL_LOCATION        (findfile "DUPL.dpf"))                                                     ;DUPL.dpf is same as MAPCLEAN>DELETE DUPLICATES>AUTOMATIC
(setq GEOS-R-TEMP_LOCATION (findfile "GEOS-R-TEMP.dpf"))                                              ;GEOS-R-TEMP.dpf is same as MAPCLEAN>BREAK CROSSING OBJECTS>AUTOMATIC, RETAIN ORIGINALS AND CREATE NEW OBJECTS ON LAYER "GEOS-R-TEMP"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; reads aloud inputted tex;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:SayIt (/ Phrase Sapi)
  (setq Phrase (getstring "\nEnter phrase: " t))
  (setq Sapi (vlax-create-object "Sapi.SpVoice"))
  (vlax-invoke Sapi "Speak" Phrase 0)
  (vlax-release-object Sapi)
  (princ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; prints number of vertices of selected entity;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:vertnum ()
   (vl-load-com)
   (setq Pl_Ent (car (entsel "\nSelect polyline: ")))
   (setq VL_Obj (vlax-ename->vla-object Pl_Ent))
   (setq endParam (vlax-curve-getEndParam Vl_Obj))
   (setq NumVert (+ 0 endParam))
   (princ (strcat "\n" (rtos NumVert 2 0) " vertices"))
   (princ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; creates a buffer with a prescribed total width around selected entity;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun C:BUFF ()
  (initget 0 "Yes No")                                                    ;sets up answer options with default (0) allowed
  (setq                                                                   ;variable definitions
    ogt (getvar 'offsetgaptype)                                           ;gets current values for offsetgaptype (rounded or sharp corners) because it will change it for its use, then change back when done
    pea (getvar 'peditaccept)                                             ;gets current values for peditaccept (whether created objects are plines or not) because it will change it for its use, then change back when done
    ends (cond ((getkword "\nDo you want curved ends? [Yes/No] <No>: ")) ("No"))  ;defualt is NO
  )
  
  (initget 0 "Yes No")                                                    ;sets up answer options with default (0) allowed
  (setq mids (cond ((getkword "\nDo you want smooth vertics? [Yes/No] <No>: ")) ("No")))  ;defualt is NO
  (initget 0 "Yes No")                                                    ;sets up answer options with default (0) allowed 
  (setq new (cond ((getkword "\n Same width? [Yes/No] <Yes>: ")) ("Yes")))        ;same width as last time?
  
  (if (= new "Yes")
    (setq buffer buffer)                                                  ;if same, set to last value entered
    (setq buffer (/ (getdist "\nBuffer width: ") 2))                      ;if not, get new value
  )                                                                       ;asks user for total (double-width) and then divides by 2 for the buffer width
  (setq esel (entsel "\nSelect object to add buffer around: "))           ;esel=entity being buffered, user input
  
  (if
    (and
      esel
      (setq
        ent (car esel)
        edata (entget ent)
        etype (cdr (assoc 0 edata))
      )
      (wcmatch etype "*LINE,ARC,CIRCLE,ELLIPSE")
      (not (wcmatch (substr (cdr (assoc 100 (reverse edata))) 5 1) "3,M"))  ; 3D Polylines and Mlines can't be Offset
    )
    (progn                                                                ;command to be carried out if above is true
      (setvar 'peditaccept 1)                                             ;created object will be apolyline
      (if (= mids "No")
        (setvar 'offsetgaptype 0)                                         ;sharp corners
        (setvar 'offsetgaptype 1)                                         ;rounded corners
      )
      (setq obj (vlax-ename->vla-object ent))
      (vla-offset obj buffer)
      (setq e1 (entlast))
      (vla-offset obj (- buffer))
      (setq e2 (entlast))
      (if (= ends "No")                                                   ;two different types of line enders
        (if (and (not (vlax-curve-isClosed ent)) (/= etype "XLINE"))      ;open-ended object other than Xline -- wrap Arcs around ends
          (progn                                                          ;then connect with plines
            (command
              "_.pline" (vlax-curve-getStartPoint e1) (vlax-curve-getStartPoint e2) ""
              "_.pline" (vlax-curve-getEndPoint e1) (vlax-curve-getEndPoint e2) ""
            )
            (if (not (wcmatch etype "SPLINE,ELLIPSE"))
              (command "_.pedit" e1 "_join" "_all" "" "")                   ;joins segments
            ); if
          ); progn [close ends]
        ); if
        (command                                                            ;else close with arcs
          "_.arc" (vlax-curve-getStartPoint e1) "_e" (vlax-curve-getStartPoint e2)
            "_direction" (angtos (+ (angle '(0 0 0) (vlax-curve-getFirstDeriv e1 (vlax-curve-getStartParam e1))) pi))
          "_.arc" (vlax-curve-getEndPoint e1) "_e" (vlax-curve-getEndPoint e2)
            "_direction" (angtos (angle '(0 0 0) (vlax-curve-getFirstDeriv e1 (vlax-curve-getEndParam e1))))
          (if (not (wcmatch etype "SPLINE,ELLIPSE"))
            (command "_.pedit" e1 "_join" "_all" "" "")                     ;joins segments
          )
        ); command
      )                                                                     ;if ends arced or not 
      (command "change" e1  "" "p" "la" "S-FENCE-TR" "")                    ;moves created object to S-FENCE-TR
    ); progn [valid object]
  ); if
  (setvar 'offsetgaptype ogt)                                               ;set back to what it was before
  (setvar 'peditaccept pea)                                                 ;set back to what it was before
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; isolates all LWPOLYLINES with 0,1,or2 vertices on training-layers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:thwomp ()
  (command "LAYER" "FREEZE" "R-PAVEMARK-LINE" "")                        ;freezes non-training layers
  (command "LAYER" "FREEZE" "GEOM-CHECKER-ARCHIVE" "")
  (command "LAYER" "FREEZE" "U-MANHOLE" "")
  (command "LAYER" "FREEZE" "U-CATCHBASIN" "")
  (command "LAYER" "FREEZE" "A-OBSTRUCTION" "")
  (command "LAYER" "FREEZE" "Contour_Line_Intermediate" "")
  (command "LAYER" "FREEZE" "Contour_Line_Major" "")
  (command "LAYER" "FREEZE" "G-TOPO-MINR" "")
  (command "LAYER" "FREEZE" "G-TOPO-MAJR" "")
  (command "LAYER" "FREEZE" "S-FENCE" "")
  (command "LAYER" "FREEZE" "0" "")
  (ssget "A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))                 ;"A"=search All unfrozen layers, attribute type 0 is layername, -4 allows operators, attribute 90 is #vertices
  (command "LAYTHW")                                                    ;thaws (unfreezes) all layers
  (command "ISOLATEOBJECTS" "P" "" ""))                                  ;isolates previous (most recent) entity selection set


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; isolates all unclosed LWPOLYLINES on training-layers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:UC ()
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
  (command "LAYTHW")                                                     ;thaws (unfreezes) all layers
  (command "ISOLATEOBJECTS" "P" "" ""))                                  ;isolates previous (most recent) entity selection set
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; fixes, then prints how many layers were changed to being by-layer;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:nb ()
 (print "Number of Non-By-Layer objects fixed:")
 (command "SetByLayer" "ALL" "" "Yes" "Yes"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; marks GEOS-R errors on layer that user has already isolated;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ger ()
  (print "This only works when you have isolated one layer you want to search for GEOS-R errors")
  (command "MAPCLEAN" GER_LOCATION))                                           ;runs saved cleanup profile called GER.dpf, which specifies putting markers on intersections


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; prints entities's layers that are not LWPOLYLINES, CIRCLES, and U-MANHOLE-TR POLYLINES; i.e. all unacceptable entities in the company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun C:bdt ()
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; calculation-heavy way of checking if any entities have similar entity attributes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun C:du ()
  (command "LAYER" "FREEZE" "R-PAVEMARK-LINE" "")                      ;freezes non-training layers
  (command "LAYER" "FREEZE" "GEOM-CHECKER-ARCHIVE" "")
  (command "LAYER" "FREEZE" "U-MANHOLE" "")
  (command "LAYER" "FREEZE" "U-CATCHBASIN" "")
  (command "LAYER" "FREEZE" "A-OBSTRUCTION" "")
  (command "LAYER" "FREEZE" "Contour_Line_Intermediate" "")
  (command "LAYER" "FREEZE" "Contour_Line_Major" "")
  (command "LAYER" "FREEZE" "G-TOPO-MINR" "")
  (command "LAYER" "FREEZE" "G-TOPO-MAJR" "")
  
  (setq SUZY (ssget "A" '((0 . "LWPOLYLINE,CIRCLE"))))                 ;"SUZY"=entity selection set containing all ("A") unfrozen LWPOLYLINES and CIRCLES
  (command "_LAYTHW")                                                  ;thaws (unfreezes) all layers
  (setq COUNTER 0)                                                     ;"COUNTER"=0, used to iterate through every object below
  (setq DU (ssadd))                                                    ;creates "DU", the entity selection set that holds one of every duplicate entity
  
  ;(setq LINELIST        (ssadd))                                      ;creating new selection sets for each entity type
  (setq LWPOLYLINELIST  (ssadd))
  (setq CIRCLELIST      (ssadd))
  ;(setq ARCLIST         (ssadd))
  ;(setq ELLIPSELIST     (ssadd))
  ;(setq SPLINELIST      (ssadd))
  ;(setq INSERTLIST      (ssadd))                                     ;INSERT is for blocks
  
  (while                  ;this block adds the entities to the selection sets
    (setq EDNA (ssname SUZY COUNTER))                                  ;"EDNA"= value COUNTER'th element in list"SUZY"
    (setq ED (entget EDNA))                                            ;"ED"=attributes of entity EDNA
    (cond                                                              ;condition of while function
  ;    ((= (cdr (assoc 0 ED)) "LINE")       (ssadd EDNA LINELIST))    ;if layername found within of "ED" (attribute list of EDNA) is "LINE", add EDNA to the list "LINELIST"
      ((= (cdr (assoc 0 ED)) "LWPOLYLINE") (ssadd EDNA LWPOLYLINELIST))
      ((= (cdr (assoc 0 ED)) "CIRCLE")     (ssadd EDNA CIRCLELIST))
  ;     ((= (cdr (assoc 0 ED)) "ARC")        (ssadd EDNA ARCLIST))
  ;     ((= (cdr (assoc 0 ED)) "ELLIPSE")    (ssadd EDNA ELLIPSELIST))
  ;     ((= (cdr (assoc 0 ED)) "SPLINE")     (ssadd EDNA SPLINELIST))
  ;     ((= (cdr (assoc 0 ED)) "INSERT")     (if (not (ASSOC 66 ED)) (ssadd EDNA SSINSERTIST)))   ;assoc 66 does something I don't understand to make attributes of blocks readable
    )
    (setq COUNTER (1+ COUNTER))                                        ;increments counter up one
  )
  
  ;(setq entityType LINELIST)        (findit)                         ;changes the search area from one entity type to the next
  (setq entityType LWPOLYLINELIST)  (findit)
  (setq entityType CIRCLELIST)      (findit)
  ;(setq entityType ARCLIST)         (findit)
  ;(setq entityType ELLIPSELIST)     (findit)
  ;(setq entityType SPLINELIST)      (findit)
  ;(setq entityType INSERTLIST)      (findit)

  (command "PSELECT" DU "" "ISOLATEOBJECTS")                           ;selects, then isolates entity selection set "DU" which has now been altered since creation
  /(print "Number of Duplicates Found:") (princ (sslength DU))          ;the end of the code, pretty self-explanatory
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;subfunction of DU, requires no input, passes nothing; makes takes one entity and checks for duplicates in list of every other entity, if found, adds entity to DU; removes entity from future search lists; moves to next entity until all have been checked
(defun findit ()                                                          
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; replaces Python script company used to use in Swagger.io; fixes non-by-layer objects; deletes duplicates; prints what layers GEOS-R errors, 2node-polylines, unclosed polylines, and bad-dxf-type errors are on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:swag ()
  (princ "Working. This should take a few seconds...THWOMP...THWOMP...THWOMP...")
  
  ;duplicates - counts number of deleted duplicates=NUMDUPLICATES-------------------------------------------------------------------------------------------------------------------------
    (setq A (sslength (ssget "X")))                                      ;counts number of entities before     
    (deletedup "B-OVERHANG")                                             ;calls subfunction "deletedup" (see below) with current search layer as the input for variable LAYCUR
    (deletedup "C-CONC-PAD")
    (deletedup "R-CURB")
    (deletedup "R-CURB-BERM")
    (deletedup "R-GUTTER")
    (deletedup "R-PAVEMARK-LINE-TR")
    (deletedup "R-PAVEMARK-POLY")
    (deletedup "R-PAVEMARK-SYMBOL")
    (deletedup "R-RAILROAD-TR")
    (deletedup "R-ROAD-ASPH")
    (deletedup "R-ROAD-CONC")
    (deletedup "R-ROAD-PAVER")
    (deletedup "R-WALK-ASPH")
    (deletedup "R-WALK-CONC")
    (deletedup "R-WALK-PAVER")
    (deletedup "U-CATCHBASIN-TR")
    (deletedup "U-MANHOLE-TR")
    (deletedup "U-SOLAR")
    (deletedup "V-VEGETATION")
    (deletedup "W-WATER")
    (setq B (sslength (ssget "X")))                                      ;counts number of entities after
    (setq NUMDUPLICATES (- A B))                                         ;"NUMDUPLICATES"=#after-#before (i.e. number of deleted duplicates)

  ;GEOS-R-errors - creates text list (not selection set) EL---------------------------------------------------------------------------------------
    (setq EL (list ": "))                                                ;"EL"=text list that will hold names of errored layers, first element (": ") is just for printout aesthetics
    (cleaner "B-OVERHANG")                                               ;calls subfunction "cleaner" (see below) with current search layer as the input for variable LAYCUR
    (cleaner "C-CONC-PAD")
    (cleaner "R-CURB")
    (cleaner "R-CURB-BERM")
    (cleaner "R-GUTTER")
    (cleaner "R-PAVEMARK-LINE-TR")
    (cleaner "R-PAVEMARK-POLY")
    (cleaner "R-PAVEMARK-SYMBOL")
    (cleaner "R-RAILROAD-TR")
    (cleaner "R-ROAD-ASPH")
    (cleaner "R-ROAD-CONC")
    (cleaner "R-ROAD-PAVER")
    (cleaner "R-WALK-ASPH")
    (cleaner "R-WALK-CONC")
    (cleaner "R-WALK-PAVER")
    (cleaner "U-CATCHBASIN-TR")
    (cleaner "U-MANHOLE-TR")
    (cleaner "U-SOLAR")
    (cleaner "V-VEGETATION")
    (cleaner "W-WATER")
  
  ;uncloseds - creates entity selection set UNCLOSEDSET-------------------------------------------------------------------------------------------------------------------------
    (command "LAYER" "FREEZE" "R-PAVEMARK-LINE" "")                      ;freezes non-training layers
    (command "LAYER" "FREEZE" "GEOM-CHECKER-ARCHIVE" "")
    (command "LAYER" "FREEZE" "U-MANHOLE" "")
    (command "LAYER" "FREEZE" "U-MANHOLE-LABEL" "")
    (command "LAYER" "FREEZE" "U-CATCHBASIN" "")
    (command "LAYER" "FREEZE" "U-DRAIN" "")
    (command "LAYER" "FREEZE" "A-OBSTRUCTION" "")
    (command "LAYER" "FREEZE" "Contour_Line_Intermediate" "")
    (command "LAYER" "FREEZE" "Contour_Line_Major" "")
    (command "LAYER" "FREEZE" "G-TOPO-MINR" "")
    (command "LAYER" "FREEZE" "G-TOPO-MAJR" "")
    (setq UNCLOSEDSET (ssget "A" '((0 . "LWPOLYLINE") (70 . 0))))        ;"UNCLOSEDSET"=entity selection set of all unclosed polylines on unfrozen ("A") layers
  
  ;2nodes - creates entity selection set TN--------------------------------------------------------------------------------------------------
    (setq TN (ssget "A" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2))))     ;"TN"= entity selection set of all unfrozen ("A") polylines
  
  ;bad-dxf-entity-types - creates entity selection set A----------------------------------------------------------------------------------
    (setq C (ssget "A"))                                                 ;"C"=entity selection set of all unfrozen ("A") entities
    (setq D (ssget "A" '((0 . "LWPOLYLINE"))))                           ;"D","E" are entity selection sets of acceptable entities
    (setq E (ssget "A" '((0 . "CIRCLE"))))
    (setq F (ssget "A" '((0 . "LWPOLYLINE") (8 . "U-MANHOLE-TR"))))      ;"F"=entity selection set of all polyline manholes, i.e. unacceptable entities
                    ;;;;;;;;;;the 3 loops below remove entity selection sets "D" and "E" from "C", then add "F" back to it
    (if (/= D nil)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
    (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
      (setq n (sslength D))                                              ;"n"=number of D (tells how many times to repeat)
      (setq i -1)                                                        ;"i"=-1 (0 at beginning of list)
      (repeat n (ssdel (ssname D (setq i (1+ i))) C))))                  ;removes ID of i'th element in list "D" from list "C", repeats until it has worked n number of times

    (if (/= E nil)                                                       
    (progn                                                               
      (setq m (sslength E))                                              
      (setq j -1)                                                        
      (repeat m (ssdel (ssname E (setq j (1+ j))) C))))                  

    (if (/= F nil)                                                       
    (progn                                                               
      (setq p (sslength F))                                              
      (setq q -1)                                                        
      (repeat p (ssadd (ssname F (setq q (1+ q))) C)))
    )                  
  
    (command "_LAYTHW")                                                  ;thaws (unfreezes) all layers
  
  ;printout--------------------------------------------------------------------------------------------------------------------------
    (command "_SetByLayer" "ALL" "" "YES" "Yes")                         ;sets all layer colors to "By-Layer"
    (print "Non-By-Layers Fixed per Output Above ^^")
    (print "# Duplicates Deleted: ") (princ NUMDUPLICATES)
    (print "GEOS-R Errors:        ") (princ (- (length EL) 1)) (princ EL)
    (print "Unclosed Polylines:   ") (printsetelements UNCLOSEDSET)  
    (print "2-Node Polys:         ") (printsetelements TN)
    (print "Bad-DXF-Entity-Types: ") (printsetelements C)
    (setq Sapi (vlax-create-object "Sapi.SpVoice")) (vlax-invoke Sapi "Speak" "thwomp" 0) (vlax-release-object Sapi) ;plays sound when command is done
    (princ)                                                              ;just a nice way to end a function in AUTOLISP (otherwise it shows stuff you don't always want)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;subfunction of swag, requires a current search layer input for LAYCUR, passes nothing, just isolates current search layer, deletes duplicates on it, then unisolates layer
(defun deletedup (LAYCUR)                                                
  (if (tblsearch "layer" LAYCUR)                                         ;if search layer doesn't exist, skip all of the subfunction "deletedup"
    (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything past first line would be "else"
      (command "LAYER" "SET" LAYCUR "OFF" "*" "Y" "ON" LAYCUR "")        ;isolates current search layer (LAYISO requires manual input, so not used here)
      (command "MAPCLEAN" DUPL_LOCATION) ;references drawing cleanup profile that deletes duplicates, regardless of layer (which is  why they're isolated)
      (command "LAYER" "ON" "*" ""))))                                   ;turns all layers back on to continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;subfunction of swag, requires a current search layer input for LAYCUR, passes nothing, just isolates current search layer & created dummy layer; if there are crossing lines, moves them to the dummy layer; if anything is on dummy layer, LAYCUR gets marked as errored; unisolates layer
(defun cleaner (LAYCUR)
  (if (tblsearch "layer" LAYCUR)                                         ;if current layer doesn't exist, skip all of the subfunction "cleaner"
  (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything after first line would be "else"
    (if (not (tblsearch "layer" "GEOS-R-TEMP"))                          ;if dummy layer doesn't exist, create it
      (command "_layer" "m" "GEOS-R-TEMP" ""))

    (command "LAYER" "SET" LAYCUR "OFF" "*" "Y" "ON" LAYCUR "ON" "GEOS-R-TEMP" "") ;isolates current search & dummy layers (LAYISO requires manual input, so not used here)

    (command "MAPCLEAN" GEOS-R-TEMP_LOCATION)                                  ;if mapclean finds errors, they are placed on dummy layer
    
    (setq newobjects (ssget "X" '((8 . "GEOS-R-TEMP"))))                 ;"NEWOBJECTS"=entity selection set of everything on dummy layer
    (if (/= newobjects nil)                                              ;if that set is not nil (empty), mark current layer as errored
      (setq EL (append EL (list LAYCUR))))
    
    (command "_laydel" "n" "GEOS-R-TEMP" "" "Y" "")                      ;delete dummy layer and turn all layers back on
    (command "_layer" "_on" "*" "")
  )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;subfunction of swag, requires entity selection set input for LAYCUR, passes nothing, just prints every entity's layer name sequentially;;;;;;;;;
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
  )
  (progn
    (princ "0:")
  )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; adds vertex to LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;user command; adds vertex to LWPOLYLINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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