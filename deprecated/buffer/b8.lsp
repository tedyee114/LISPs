(defun C:B8 ()
  (setq
    ogt (getvar 'offsetgaptype)
    pea (getvar 'peditaccept)
    buffer (/ (getdist "\nBuffer width: ") 2)
    esel (entsel "\nSelect object to add buffer around: ")
  ); setq
  (if
    (and
      esel
      (setq
        ent (car esel)
        edata (entget ent)
        etype (cdr (assoc 0 edata))
      ); setq
      (wcmatch etype "*LINE,ARC,CIRCLE,ELLIPSE")
      (not (wcmatch (substr (cdr (assoc 100 (reverse edata))) 5 1) "3,M"))
        ; 3D Polylines and Mlines can't be Offset
    ); and
    (progn ; then
      (setvar 'offsetgaptype 0); rounded corners
      (setvar 'peditaccept 1)

      (setq obj (vlax-ename->vla-object ent))
      (vla-offset obj buffer)
      (setq e1 (entlast))
      (vla-offset obj (- buffer))
      (setq e2 (entlast))
      (if (and (not (vlax-curve-isClosed ent)) (/= etype "XLINE"))
        ; open-ended object other than Xline -- wrap Arcs around ends
        (progn ; then
          (command
            "_.pline" (e1) "_e" (e2)
              ;; "_direction" (angtos (+ (angle '(0 0 0) (vlax-curve-getFirstDeriv e1 (vlax-curve-getStartParam e1))) pi))
            "_.pline" (e1) "_e" (e2)
              ;; "_direction" (angtos (angle '(0 0 0) (vlax-curve-getFirstDeriv e1 (vlax-curve-getEndParam e1))))
          ); command
          (if (not (wcmatch etype "SPLINE,ELLIPSE"))

            (command "_.pedit" e1 "_join" "_all" "" ""); connect them
          ); if
        ); progn [close ends]
      ); if
    ); progn [valid object]
  ); if
  (setvar 'offsetgaptype ogt)
  (setvar 'peditaccept pea)
  (princ)
); defun