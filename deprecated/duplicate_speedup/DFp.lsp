(defun C:DFP ()
    (print "Running...")
    (setq BADLAYERLIST (list ": "))
    (ducleaner "B-OVERHANG")                        ;sets current search layer, then runs subfunction called "ducleaner"
    (ducleaner "C-CONC-PAD")
    (ducleaner "R-CURB")
    (ducleaner "R-CURB-BERM")
    (ducleaner "R-GUTTER")
    (ducleaner "R-PAVEMARK-LINE-TR")
    (ducleaner "R-PAVEMARK-POLY")
    (ducleaner "R-PAVEMARK-SYMBOL")
    (ducleaner "R-RAILROAD-TR")
    (ducleaner "R-ROAD-ASPH")
    (ducleaner "R-ROAD-CONC")
    (ducleaner "R-ROAD-PAVER")
    (ducleaner "R-WALK-ASPH")
    (ducleaner "R-WALK-CONC")
    (ducleaner "R-WALK-PAVER")
    (ducleaner "U-CATCHBASIN-TR")
    (ducleaner "U-MANHOLE-TR")
    (ducleaner "U-SOLAR")
    (ducleaner "V-VEGETATION")
    (ducleaner "W-WATER")
    
    (princ BADLAYERLIST)
    (princ)
)


(defun ducleaner (LAYCUR)
  (if (tblsearch "layer" LAYCUR)                                           ;if current layer doesn't exist, skip all of the subfunction "ducleaner"
    (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything in first line would be "else"
      (command "_layer" "set" LAYCUR "")                                   ;this block turns off everything except current layer
      (command "_layer" "_off" "*" "N" "")
      (command "_layer" "_on" LAYCUR "")

      (setq A (ssget "X" '((8 . "LAYCUR"))))                 ;"A"= entity selection set of all objects on current search layer
      (command "_mapclean" "C:Users\ted_airworks.io\Documents\Scripts\LISPs\DUPL.dpf")                                   ;if mapclean finds errors, they are deleted
      (setq B (ssget "X" '((8 . "LAYCUR"))))                 ;"B"= entity selection set of all objects on current search layer after deletions

      (if (/= A B)                                              ;if any have been deledted (A/=B), mark current layer
       (setq BADLAYERLIST (append BADLAYERLIST (list LAYCUR))))
      
      (command "_layer" "_on" "*" "")
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
