(defun C:DFk ()
    (print "Running...")
    (setq BADLAYERLIST (list ": "))
    (setq LAYCUR "B-OVERHANG")          (ducleaner)                        ;sets current search layer, then runs subfunction called "ducleaner"
    (setq LAYCUR "C-CONC-PAD")          (ducleaner)
    (setq LAYCUR "R-CURB")              (ducleaner)
    (setq LAYCUR "R-CURB-BERM")         (ducleaner)
    (setq LAYCUR "R-GUTTER")            (ducleaner)
    (setq LAYCUR "R-PAVEMARK-LINE-TR")  (ducleaner)
    (setq LAYCUR "R-PAVEMARK-POLY")     (ducleaner)
    (setq LAYCUR "R-PAVEMARK-SYMBOL")   (ducleaner)
    (setq LAYCUR "R-RAILROAD-TR")       (ducleaner)
    (setq LAYCUR "R-ROAD-ASPH")         (ducleaner)
    (setq LAYCUR "R-ROAD-CONC")         (ducleaner)
    (setq LAYCUR "R-ROAD-PAVER")        (ducleaner)
    (setq LAYCUR "R-WALK-ASPH")         (ducleaner)
    (setq LAYCUR "R-WALK-CONC")         (ducleaner)
    (setq LAYCUR "R-WALK-PAVER")        (ducleaner)
    (setq LAYCUR "U-CATCHBASIN-TR")     (ducleaner)
    (setq LAYCUR "U-MANHOLE-TR")        (ducleaner)
    (setq LAYCUR "U-SOLAR")             (ducleaner)
    (setq LAYCUR "V-VEGETATION")        (ducleaner)
    (setq LAYCUR "W-WATER")             (ducleaner)
    
    (princ BADLAYERLIST)
    (princ)
)


(defun ducleaner ()
  (if (tblsearch "layer" LAYCUR)                                         ;if current layer doesn't exist, skip all of the subfunction "ducleaner"
    (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything REMAINING first line would be "else"
      (if (not (tblsearch "layer" "ERRORS-TEMP"))                          ;if dummy layer doesn't exist, create it
        (command "_layer" "m" "ERRORS-TEMP" ""))
      
      (command "_layer" "set" LAYCUR "")                                   ;this block turns off everything except current layer and dummy layer
      (command "_layer" "_off" "*" "N" "")
      (command "_layer" "_on" LAYCUR "_on" "ERRORS-TEMP" "")

      (command "_mapclean" "C:\Users\ted_airworks.io\Documents\Scripts\LISPs\DUPLICATES")                                   ;if mapclean finds errors, they are placed on dummy layer
      
      (setq newobjects (ssget "X" '((8 . "ERRORS-TEMP"))))                 ;"NEWOBJECTS"=entity selection set of everything on dummy layer
      (if (/= newobjects nil)                                              ;if that set is not nil (empty), mark current layer as errored
       (setq BADLAYERLIST (append BADLAYERLIST (list LAYCUR))))
      
      (command "_laydel" "n" "ERRORS-TEMP" "" "Y" "")                      ;delete dummy layer and turn all layers back on
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