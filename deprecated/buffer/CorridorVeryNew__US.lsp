 
;;; 
;;; Minimum US/English translation by Patrice
;;; 

;;; 
;;; Nouvelle version de la routine CORRIDOR
;;; CORRIDOR2.LSP le 09/07/2007
;;; Taper au clavier:  CORRIDORVERYNEW
;;;
;;; ATTENTION: Pour ACAD/MAP 2007/2008/2009
;;; Ne fonctionne pas sur versions precedentes 
;;; 
;;; Routine :  CORRIDORVERYNEW from Gilles (gile)
;;; 


(defun c:corridorverynew (/      *error*       JoinPlines    HatchPline    AcDoc
                      Space  sort   inc    ht     ss    extr   col    long
                      larg   pl0    nor    pl1    pl2    ps1    ps2    nb
                      n      pt0    pa0    pt1    pt2    cut1   cut2   txt
                      box
                     )

  (vl-load-com)

  ;; Redéfintion de *error* (fermeture du groupe d'annulation)
  (defun *error* (msg)
    (if (= msg "Fonction annulée")
      (princ)
      (princ (strcat "\nErreur: " msg))
    )
    (vla-endundomark
      (vla-get-activedocument (vlax-get-acad-object))
    )
    (princ)
  )

  ;; Joint deux polylignes en une polyligne fermée
  (defun JoinPlines (p1 p2 / v1 v2 i lst pl)
    (setq v1 (fix (vlax-curve-getEndParam p1))
          v2 (fix (vlax-curve-getEndParam p2))
          i  0
    )
    (repeat v1
      (setq lst (cons (cons i (vla-getBulge p1 i)) lst)
            i   (1+ i)
      )
    )
    (setq i (1+ i))
    (repeat v2
      (setq lst (cons (cons i (- (vla-GetBulge p2 (setq v2 (1- v2))))) lst)
            i   (1+ i)
      )
    )
    (setq pl
           (vlax-invoke
             Space
             'addLightWeightPolyline
             (append (vlax-get p1 'Coordinates)
                     (apply 'append
                            (reverse (split-list (vlax-get p2 'Coordinates) 2))
                     )
             )
           )
    )
    (vla-put-Closed pl :vlax-true)
    (mapcar '(lambda (x) (vla-SetBulge pl (car x) (cdr x))) lst)
    (vla-put-Normal pl (vla-get-Normal p1))
    (vla-put-Elevation pl (vla-get-Elevation p1))
    (vla-delete p1)
    (vla-delete p2)
    pl
  )

  ;; hachure une polyligne (SOLID)
  (defun HatchPline (pl / hatch)
    (setq hatch (vla-AddHatch
                  Space
                  acHatchPatternTypePredefined
                  "SOLID"
                  :vlax-true
                )
    )
    (vlax-invoke hatch 'AppendOuterLoop (list pl))
    (vla-put-Color hatch col)
    (vlax-invoke sort 'MoveToBottom (list hatch))
  )

  ;; Fonction principale
  (setq AcDoc (vla-get-ActiveDocument (vlax-get-acad-object))
        Space (if (= (getvar "CVPORT") 1)
                (vla-get-PaperSpace AcDoc)
                (vla-get-ModelSpace AcDoc)
              )
  )
  (or (vlax-ldata-get "corridor" "long")
      (vlax-ldata-put "corridor" "long" 40.0)
  )
  (or (vlax-ldata-get "corridor" "larg")
      (vlax-ldata-put "corridor" "larg" 20.0)
  )
  (or (vlax-ldata-get "corridor" "num")
      (vlax-ldata-put "corridor" "num" 1)
  ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (initget "Oui Non")
  (initget "Yes No")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (if

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   (/= "Non"
;       (getkword "\Numéroter les boites ? [Oui/Non] <O>: ") 
    (/= "No"
        (getkword "\Numbering Boxes ? [Yes/No] <Y>: ") 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

    )
     (progn
       (if (setq inc 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;                  (getint (strcat "\nEntrez le numéro de départ <" 
                   (getint (strcat "\nEnter First Number <"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                  (itoa (vlax-ldata-get "corridor" "num"))
                                  ">: "
                          )
                  )
           )
         (vlax-ldata-put "corridor" "num" inc)
         (setq inc (vlax-ldata-get "corridor" "num"))
       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      (if (setq ht (getdist (strcat "\nSpécifiez la hauteur de texte <"
       (if (setq ht (getdist (strcat "\nSpecify Text Height <"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                     (rtos (getvar "TEXTSIZE"))
                                     ">: "
                             )
                    )
           )
         (setvar "TEXTSIZE" ht)
         (setq ht (getvar "TEXTSIZE"))
       )
     )
  ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (initget "Oui Non") 
  (initget "Yes No" )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setq extr (getkword "\nBoites aux extrémités seulement ? [Oui/Non] <O>: "))
  (setq extr (getkword "\nBoxes at the End only ? [Yes/No] <Y>: "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (initget 6)
  (if (setq larg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;            (getdist (strcat "\nLargeur/Emprise des boites <"
             (getdist (strcat "\nBoxes Width <"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                              (rtos (vlax-ldata-get "corridor" "larg"))
                              ">: "
                      )
             )
      )
    (vlax-ldata-put "corridor" "larg" larg)
    (setq larg (vlax-ldata-get "corridor" "larg"))
  ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (if (= "Non" extr)
  (if (= "No" extr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (progn
      (initget 6)
      (if (setq long

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                (getdist (strcat "\nLongueur des boites <"
                 (getdist (strcat "\nBoxes Lengths <"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                  (rtos (vlax-ldata-get "corridor" "long"))
                                  ">: "
                          )
                 )
          )
        (vlax-ldata-put "corridor" "long" long)
        (setq long (vlax-ldata-get "corridor" "long"))
      )
    )
    (progn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     (initget "Oui Non")
      (initget "Yes No")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     (if (/= "Non" (getkword "\nHachurer ? [Oui/Non] <O>: "))
      (if (/= "No"  (getkword "\nHatch ? [Yes/No] <Y>: "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (progn
          (if (vl-catch-all-error-p
                (setq sort (vl-catch-all-apply
                             'vla-getObject
                             (list (vla-getExtensionDictionary
                                     space
                                   )
                                   "ACAD_SORTENTS"
                             )
                           )
                )
              )
            (setq sort (vla-addObject
                         (vla-getExtensionDictionary
                           space
                         )
                         "ACAD_SORTENTS"
                         "AcDbSortentsTable"
                       )
            )
          )
          (while (not (setq col (acad_colordlg 3))))
        )
      )
    )
  )
  (if (ssget '((0 . "LWPOLYLINE")))
    (progn
      (vla-StartUndoMark AcDoc)
      (vlax-for pl0 (setq ss (vla-get-ActiveSelectionSet AcDoc))
        (setq nor (vlax-get pl0 'Normal)
              pl1 (car (vlax-invoke pl0 'Offset (/ larg 2.0)))
              pl2 (car (vlax-invoke pl0 'Offset (/ larg -2.0)))
              ps1 (trans (vlax-curve-getPointAtParam pl1 0) 0 nor)
              ps2 (trans (vlax-curve-getPointAtParam pl2 0) 0 nor)
        )
        (and inc (setq inc (vlax-ldata-get "corridor" "num"))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       (if (= "Non" extr)
        (if (= "No" extr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (progn
            (setq nb (fix
                       (/ (vlax-curve-getDistAtParam
                            pl0
                            (vlax-curve-getEndParam pl0)
                          )
                          long
                       )
                     )
                  n  1
            )
            (repeat nb
              (setq pt0 (vlax-curve-getPointAtDist pl0 (* n long))
                    pa0 (vlax-curve-getParamatpoint pl0 pt0)
              )
              (if (equal pa0 (fix pa0) 1e-9)
                (setq pt1 (vlax-curve-getPointatParam pl1 1)
                      pt2 (vlax-curve-getPointatParam pl2 1)
                )
                (setq pt1 (vlax-curve-getClosestPointTo pl1 pt0)
                      pt2 (vlax-curve-getClosestPointTo pl2 pt0)
                )
              )
              (setq cut1 (CutPlineAtPoint pl1 pt1)
                    cut2 (CutPlineAtPoint pl2 pt2)
              )
              (cond
                ((not (car cut1))
                 (vlax-put pl2
                           'Coordinates
                           (append (vlax-get pl2 'Coordinates)
                                   (reverse (cdr (reverse (trans pt1 0 nor))))
                           )
                 )
                 (vla-put-Closed pl2 :vlax-true)
                 (vla-put-Layer pl2 (getvar "CLAYER"))
                )
                ((not (car cut2))
                 (vlax-put pl1
                           'Coordinates
                           (append (vlax-get pl1 'Coordinates)
                                   (reverse (cdr (reverse (trans pt2 0 nor))))
                           )
                 )
                 (vla-put-Closed pl1 :vlax-true)
                 (vla-put-Layer pl1 (getvar "CLAYER"))
                )
                (T (JoinPlines (car cut1) (car cut2)))
              )
              (if inc
                (progn
                  (setq txt
                            (vla-addText
                              Space
                              (itoa inc)
                              (vlax-3d-point '(0 0 0))
                              ht
                            )
                  )
                  (vla-put-Normal txt (vlax-3d-point nor))
                  (vla-put-Alignment txt 10)
                  (vla-put-TextAlignmentPoint
                    txt
                    (vlax-3d-point
                      (vlax-curve-getPointAtDist pl0 (- (* n long) (/ long 2)))
                    )
                  )
                  (setq inc (1+ inc))
                )
              )
              (setq n   (1+ n)
                    pl1 (cadr cut1)
                    pl2 (cadr cut2)
              )
            )
            (cond
              ((not pl1)
               (vlax-put pl2
                         'Coordinates
                         (append (vlax-get pl2 'Coordinates)
                                 (list (car ps1) (cadr ps1))
                         )
               )
               (vla-put-Closed pl2 :vlax-true)
               (vla-put-Layer pl2 (getvar "CLAYER"))
              )
              ((not pl2)
               (vlax-put pl1
                         'Coordinates
                         (append (vlax-get pl1 'Coordinates)
                                 (list (car ps2) (cadr ps2))
                         )
               )
               (vla-put-Closed pl1 :vlax-true)
               (vla-put-Layer pl1 (getvar "CLAYER"))
              )
              (T (JoinPlines pl1 pl2))
            )
            (if inc
              (progn
                (setq txt
                       (vla-addText
                         Space
                         (itoa inc)
                         (vlax-3d-point '(0 0 0))
                         ht
                       )
                )
                (vla-put-Normal txt (vlax-3d-point nor))
                (vla-put-Alignment txt 10)
                (vla-put-TextAlignmentPoint
                  txt
                  (vlax-3d-point
                    (vlax-curve-getPointAtDist
                      pl0
                      (/ (+ (vlax-curve-getDistatPoint pl0 pt0)
                            (vlax-curve-getDistAtParam
                              pl0
                              (vlax-curve-getEndParam pl0)
                            )
                         )
                         2.0
                      )
                    )
                  )
                )
                ;;(vlax-ldata-put "corridor" "num" (1+ inc))
              )
            )
          )
          (progn
            (setq nb (1- (fix (vlax-curve-getEndParam pl0)))
                  n  1
            )
            (repeat nb
              (setq pt1  (vlax-curve-getPointatParam pl1 1)
                    pt2  (vlax-curve-getPointatParam pl2 1)
                    cut1 (CutPlineAtPoint pl1 pt1)
                    cut2 (CutPlineAtPoint pl2 pt2)
              )
              (cond
                ((not (car cut1))
                 (vlax-put pl2
                           'Coordinates
                           (append (vlax-get pl2 'Coordinates)
                                   (reverse (cdr (reverse (trans pt1 0 nor))))
                           )
                 )
                 (vla-put-Closed pl2 :vlax-true)
                 (vla-put-Layer pl2 (getvar "CLAYER"))
                 (setq box pl2)
                )
                ((not (car cut2))
                 (vlax-put pl1
                           'Coordinates
                           (append (vlax-get pl1 'Coordinates)
                                   (reverse (cdr (reverse (trans pt2 0 nor))))
                           )
                 )
                 (vla-put-Closed pl1 :vlax-true)
                 (vla-put-Layer pl1 (getvar "CLAYER"))
                 (setq box pl1)
                )
                (T (setq box (JoinPlines (car cut1) (car cut2))))
              )
              (if inc
                (progn
                  (setq txt
                         (vla-addText
                           Space
                           (itoa inc)
                           (vlax-3d-point '(0 0 0))
                           ht
                         )
                  )
                  (vla-put-Normal txt (vlax-3d-point nor))
                  (vla-put-Alignment txt 10)
                  (vla-put-TextAlignmentPoint
                    txt
                    (vlax-3d-point
                      (vlax-curve-getPointAtParam pl0 (- n 0.5))
                    )
                  )
                  (setq inc (1+ inc))
                )
              )
              (if col
                (HatchPline box)
              )
              (setq n   (1+ n)
                    pl1 (cadr cut1)
                    pl2 (cadr cut2)
              )
            )
            (cond
              ((not pl1)
               (vlax-put pl2
                         'Coordinates
                         (append (vlax-get pl2 'Coordinates)
                                 (list (car ps1) (cadr ps1))
                         )
               )
               (vla-put-Closed pl2 :vlax-true)
               (vla-put-Layer pl2 (getvar "CLAYER"))
               (setq box pl2)
              )
              ((not pl2)
               (vlax-put pl1
                         'Coordinates
                         (append (vlax-get pl1 'Coordinates)
                                 (list (car ps2) (cadr ps2))
                         )
               )
               (vla-put-Closed pl1 :vlax-true)
               (vla-put-Layer pl1 (getvar "CLAYER"))
               (setq box pl1)
              )
              (T (setq box (JoinPlines pl1 pl2)))
            )
            (if inc
              (progn
                (setq txt
                       (vla-addText
                         Space
                         (itoa inc)
                         (vlax-3d-point '(0 0 0))
                         ht
                       )
                )
                (vla-put-Normal txt (vlax-3d-point nor))
                (vla-put-Alignment txt 10)
                (vla-put-TextAlignmentPoint
                  txt
                  (vlax-3d-point
                    (vlax-curve-getPointAtParam pl0 (- n 0.5))
                  )
                )
                ;;(vlax-ldata-put "corridor" "num" (1+ inc))
              )
            )
            (if col
              (HatchPline box)
            )
          )
        )
      )
      (vla-delete ss)
      (vla-EndUndoMark AcDoc)
    )
  )
  (princ)
)


;;;************************* SOUS ROUTINES *************************;;;

;;; Angle2Bulge
;;; Retourne le bulge correspondant à un angle

(defun Angle2Bulge (a)
  (/ (sin (/ a 4.0)) (cos (/ a 4.0)))
)

;;; ArcCenterBy3Points
;;; Retourne le centre de l'arc décrit par 3 points

(defun ArcCenterBy3Points (p1 p2 p3)
  ((lambda (mid1 mid2)
     (inters mid1
             (polar mid1 (+ (angle p1 p2) (/ pi 2)) 1.0)
             mid2
             (polar mid2 (+ (angle p2 p3) (/ pi 2)) 1.0)
             nil
     )
   )
    (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0)) p1 p2)
    (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0)) p2 p3)
  )
)

;;; SUBLST Retourne une sous-liste
;;; Premier élément : 1
;;; (sublst '(1 2 3 4 5 6) 3 2) -> (3 4)
;;; (sublst '(1 2 3 4 5 6) 3 -1) -> (3 4 5 6)
;;; (sublst '(1 2 3 4 5 6) 3 12) -> (3 4 5 6)
;;; (sublst '(1 2 3 4 5 6) 3 nil) -> (3 4 5 6)

(defun sublst (lst start leng / rslt)
  (or (<= 1 leng (- (length lst) start))
      (setq leng (- (length lst) (1- start)))
  )
  (repeat leng
    (setq rslt  (cons (nth (1- start) lst) rslt)
          start (1+ start)
    )
  )
  (reverse rslt)
)

;; SPLIT-LIST Retourne une liste de sous-listes
;; Arguments
;; - lst : la liste à fractionner
;; - num : un entier, le nombre d'éléments des sous listes
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

;;; CutPlineAtPoint
;;; Coupe la polyligne au point spécifié et retourne la liste des deux objets générés
;;; (ename ou vla-object selon le type de l'argument pl)
;;; 
;;; Arguments
;;; pl : la polyligne à couper (ename ou vla-object)
;;; pt : le point de coupure sur la polyligne (coordonnées SCG)

(defun CutPlineAtPoint
                       (pl pt / en no pa p0 p1 pn cl l0 l1 l2 ce sp c b0 b1 b2
                        bp a1 a2 n wp w0 w1 w2)
  (vl-load-com)
  (or (= (type pl) 'VLA-OBJECT)
      (setq pl (vlax-ename->vla-object pl)
            en T
      )
  )
  (setq no (vlax-get pl 'Normal)
        pa (fix (vlax-curve-getParamAtPoint pl pt))
        p0 (vlax-curve-getPointAtparam pl pa)
        p1 (vlax-curve-getPointAtParam pl (1+ pa))
        pn (reverse (cdr (reverse (trans pt 0 no))))
        cl (vla-Copy pl)
        l0 (vlax-get pl 'Coordinates)
        l1 (append (sublst l0 1 (* 2 (1+ pa))) pn)
        l2 (append pn (sublst l0 (1+ (* 2 (1+ pa))) nil))
        ce (if (not (equal pt p0 1e-9))
             (ArcCenterBy3Points (trans p0 0 no) pn (trans p1 0 no))
           )
        sp (reverse
             (cdr (reverse (trans (vlax-curve-getStartPoint pl) 0 no)))
           )
  )
  (and (= (vla-get-Closed pl) :vlax-true)
       (setq c  T
             l2 (append l2 sp)
       )
  )
  (repeat (setq n (if c
                    (fix (vlax-curve-getendParam pl))
                    (fix (1+ (vlax-curve-getendParam pl)))
                  )
          )
    (setq b0 (cons (vla-getBulge pl (setq n (1- n))) b0))
    (vla-GetWidth pl n 'StartWidth 'EndWidth)
    (setq w0 (cons (list StartWidth EndWidth) w0))
  )
  (setq bp (nth pa b0))
  (if ce
    (progn
      (setq a1 (- (angle ce pn) (angle ce (trans p0 0 no)))
            a2 (- (angle ce (trans p1 0 no)) (angle ce pn))
      )
      (if (minusp bp)
        (foreach a '(a1 a2)
          (if (< 0 (eval a))
            (set a (- (eval a) (* 2 pi)))
          )
        )
        (foreach a '(a1 a2)
          (if (< (eval a) 0)
            (set a (+ (eval a) (* 2 pi)))
          )
        )
      )
    )
  )
  (setq b1 (append
             (if (zerop pa)
               nil
               (sublst b0 1 pa)
             )
             (if ce
               (list (Angle2Bulge a1))
               (list bp)
             )
           )
        b2 (append
             (if ce
               (list (Angle2Bulge a2))
               (list bp)
             )
             (sublst b0 (+ 2 pa) nil)
           )
        wp (if (equal pt p0 1e-9)
             (car (nth pa w0))
             (+ (car (nth pa w0))
                (* (- (cadr (nth pa w0)) (car (nth pa w0)))
                   (/ (- (vlax-curve-getDistAtPoint pl pt)
                         (vlax-curve-getDistAtParam pl pa)
                      )
                      (- (vlax-curve-getDistAtParam pl (1+ pa))
                         (vlax-curve-getDistAtParam pl pa)
                      )
                   )
                )
             )
           )
        w1 (append (if (zerop pa)
                     nil
                     (sublst w0 1 pa)
                   )
                   (list (list (car (nth pa w0)) wp))
           )
        w2 (append (list (list wp (cadr (nth pa w0))))
                   (sublst w0 (+ 2 pa) nil)
           )
  )
  (if c
    (progn
      (vla-put-Closed pl :vlax-false)
      (vla-put-Closed cl :vlax-false)
    )
  )
  (mapcar '(lambda (p l b w)
             (vlax-put p 'Coordinates l)
             (repeat (setq n (length b))
               (vla-SetBulge p (setq n (1- n)) (nth n b))
             )
             (repeat (setq n (length w))
               (vla-SetWidth
                 p
                 (setq n (1- n))
                 (car (nth n w))
                 (cadr (nth n w))
               )
             )
           )
          (list pl cl)
          (list l1 l2)
          (list b1 b2)
          (list w1 w2)
  )
  (if en
    (list (vlax-vla-object->ename pl)
          (vlax-vla-object->ename pl)
    )
    (list pl cl)
  )
)

(princ "\nTaper au clavier :   CORRIDORVERYNEW \n")

