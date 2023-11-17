(defun C:DFf ()
  (setq BEFORE (sslength (ssget "X")))
  (command "OVERKILL" "ALL" "" "" "")
  (setq REMAINING (sslength (ssget "X")))
  (princ (- BEFORE REMAINING)) (princ "duplicate objects deleted")
  (princ)
  ;; (if (/= BEFORE REMAINING)                                                       ;avoids interrupt errors, sslength of an empty set returns nil and pauses code
  ;;   (progn                                                               ;everything inside of progn parentheses is "then", otherwise everything REMAINING first line would be "else"
  ;;     (setq n (sslength REMAINING))                                              ;"n"=number of B
  ;;     (setq i -1)                                                        ;"i"=-1 (0 at beginning of list)
  ;;     (repeat n (ssdel (ssname REMAINING (setq i (1+ i))) BEFORE))                  ;removes ID of i'th element in list "B" from list "A", repeats until it has worked n number of times
  ;;   )
  ;; )
  ;;  (printsetelements BEFORE)
)



;(command "_mapclean" "DUPLICATES")
  ;(setq DU (ssget "X" '((8 . "ERRORS-TEMP"))))
   ;  (print "duplicates:        ")  (printsetelements DU)




                                                ;makes a list that will hold names of layers with errors, first element is just so that it prints a colon at the end
    ;; (setq LAYCUR "B-OVERHANG")          (cleaner)                        ;sets current search layer, then runs subfunction called "cleaner"
    ;; (setq LAYCUR "C-CONC-PAD")          (cleaner)
    ;; (setq LAYCUR "R-CURB")              (cleaner)
    ;; (setq LAYCUR "R-CURB-BERM")         (cleaner)
    ;; (setq LAYCUR "R-GUTTER")            (cleaner)
    ;; (setq LAYCUR "R-PAVEMARK-LINE-TR")  (cleaner)
    ;; (setq LAYCUR "R-PAVEMARK-POLY")     (cleaner)
    ;; (setq LAYCUR "R-PAVEMARK-SYMBOL")   (cleaner)
    ;; (setq LAYCUR "R-RAILROAD-TR")       (cleaner)
    ;; (setq LAYCUR "R-ROAD-ASPH")         (cleaner)
    ;; (setq LAYCUR "R-ROAD-CONC")         (cleaner)
    ;; (setq LAYCUR "R-ROAD-PAVER")        (cleaner)
    ;; (setq LAYCUR "R-WALK-ASPH")         (cleaner)
    ;; (setq LAYCUR "R-WALK-CONC")         (cleaner)
    ;; (setq LAYCUR "R-WALK-PAVER")        (cleaner)
    ;; (setq LAYCUR "U-CATCHBASIN-TR")     (cleaner)
    ;; (setq LAYCUR "U-MANHOLE-TR")        (cleaner)
    ;; (setq LAYCUR "U-SOLAR")             (cleaner)
    ;; (setq LAYCUR "V-VEGETATION")        (cleaner)
    ;; (setq LAYCUR "W-WATER")             (cleaner)


(defun cleaner ()
  (if (tblsearch "layer" LAYCUR)                                         ;if current layer doesn't exist, skip all of the subfunction "cleaner"
  (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything REMAINING first line would be "else"
    (if (not (tblsearch "layer" "ERRORS-TEMP"))                          ;if dummy layer doesn't exist, create it
      (command "_layer" "m" "ERRORS-TEMP" ""))
    
    (command "_layer" "set" LAYCUR "")                                   ;this block turns off everything except current layer and dummy layer
    (command "_layer" "_off" "*" "N" "")
    (command "_layer" "_on" LAYCUR "_on" "ERRORS-TEMP" "")

    (command "_mapclean" "DUPLICATES")                                   ;if mapclean finds errors, they are placed on dummy layer
    
    ;(setq newobjects (ssget "X" '((8 . "ERRORS-TEMP"))))                 ;"NEWOBJECTS"=entity selection set of everything on dummy layer
    ;(if (/= newobjects nil)                                              ;if that set is not nil (empty), mark current layer as errored
    ;  (setq EL (append EL (list LAYCUR))))
    
    ;(command "_laydel" "n" "ERRORS-TEMP" "" "Y" "")                      ;delete dummy layer and turn all layers back on
    (command "_layer" "_on" "*" "")
  )
  )
)