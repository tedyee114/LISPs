(defun C:DFb ()
    (print "Running...")
    (setq BADLAYERLIST (list ": "))
    (dufinder "B-OVERHANG")                        ;sets current search layer, then runs subfunction called "dufinder"
    (dufinder "C-CONC-PAD")
    (dufinder "R-CURB")
    (dufinder "R-CURB-BERM")
    (dufinder "R-GUTTER")
    (dufinder "R-PAVEMARK-LINE-TR")
    (dufinder "R-PAVEMARK-POLY")
    (dufinder "R-PAVEMARK-SYMBOL")
    (dufinder "R-RAILROAD-TR")
    (dufinder "R-ROAD-ASPH")
    (dufinder "R-ROAD-CONC")
    (dufinder "R-ROAD-PAVER")
    (dufinder "R-WALK-ASPH")
    (dufinder "R-WALK-CONC")
    (dufinder "R-WALK-PAVER")
    (dufinder "U-CATCHBASIN-TR")
    (dufinder "U-MANHOLE-TR")
    (dufinder "U-SOLAR")
    (dufinder "V-VEGETATION")
    (dufinder "W-WATER")
    
    (princ BADLAYERLIST)
    (princ)
)


(defun dufinder (LAYCUR)
  (if (tblsearch "layer" LAYCUR)                                           ;if current layer doesn't exist, skip all of the subfunction "dufinder"
    (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything in first line would be "else"
      ;(command "LAYER" "SET" LAYCUR "OFF" "*" "Y" "ON" LAYCUR "")          ;isolates current search layer (LAYISO requires manual input, so not used)
      (command "_layer" "set" LAYCUR "")                                   ;this block turns off everything except current layer and dummy layer
      (command "_layer" "_off" "*" "N" "")
      (command "_layer" "_on" LAYCUR "")

      (setq A (ssget "X" '((8 . "LAYCUR"))))           ;"A"= entity selection set of all objects on current search layer
      (if (/= A nil)
      (princ (sslength A)))
      (command "_mapclean" "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\DUPL.dpf")                                   ;if mapclean finds errors, they are deleted
      (setq B (ssget "X" '((8 . "LAYCUR"))))          ;"B"= entity selection set of all objects on current search layer after deletions
      (if (/= B nil)
      (princ (sslength B)))
      
      (if (/= A B)                                                         ;if any have been deledted (A/=B), mark current layer
       (append BADLAYERLIST (list LAYCUR)))
      
      (command "LAYER" "ON" "*" "")
    )
  )
)


;; (setq BEFORE (sslength (ssget "X")))
;;   (command "OVERKILL" "ALL" "" "" "")
;;   (setq REMAINING (sslength (ssget "X")))
;;   (princ Before)
;;   (princ REMAINING)
;;   ;(princ (- BEFORE REMAINING)) (princ "duplicate objects deleted")
;;   (princ)

  ;; (if (/= BEFORE REMAINING)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
  ;;   (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything REMAINING first line would be "else"
  ;;     (setq n (sslength REMAINING))                                              ;"n"=number of B
  ;;     (setq i -1)                                                        ;"i"=-1 (0 at beginning of list)
  ;;     (repeat n (ssdel (ssname REMAINING (setq i (1+ i))) BEFORE))                  ;removes ID of i'th element in list "B" from list "A", repeats until it has worked n number of times
  ;;   )
  ;; )
  ;;  (printsetelements BEFORE)
;(command "LAYER" "SET" B-OVERHANG "OFF" "*" "N" "ON" B-OVERHANG "") (setq A (ssget "X" '((8 . "B-OVERHANG"))))