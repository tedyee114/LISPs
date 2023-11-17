(defun C:B22 ()
  (initget 0 "Yes No")                                                    ;sets up answer options with default (0) allowed
  (setq                                                                   ;variable definitions
    ogt (getvar 'offsetgaptype)                                           ;gets current values for offsetgaptype (rounded or sharp corners) because it will change it for its use, then change back when done
    pea (getvar 'peditaccept)                                             ;gets current values for peditaccept (whether created objects are plines or not) because it will change it for its use, then change back when done
    ends (cond ((getkword "\nDo you want curved ends? [Yes/No] <No>: ")) ("No"))  ;defualt is NO
  )
  (initget 0 "Yes No")                                                    ;sets up answer options with default (0) allowed
  (setq
    mids (cond ((getkword "\nDo you want smooth vertics? [Yes/Space] <No>: ")) ("No"))  ;defualt is NO
    buffer (/ (getdist "\nBuffer width: ") 2)                             ;asks user for total (double-width) and then divides by 2 for the buffer width
    esel (entsel "\nSelect object to add buffer around: ")                ;esel=entity being buffered, user input
  )
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
      (COMMAND "_.chprop" e1 "_LAYER" "S-FENCE-TR" "" "")
    ); progn [valid object]
  ); if
  (setvar 'offsetgaptype ogt)                                               ;set back to what it was before
  (setvar 'peditaccept pea)                                                 ;set back to what it was before
  (princ)
)