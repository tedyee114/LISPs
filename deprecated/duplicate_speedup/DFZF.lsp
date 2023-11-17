(defun C:DFZF ()
    (print "Running...")
    (setq A (ssget "X"))
  
    (deletedup "B-OVERHANG")                        ;sets current search layer, then runs subfunction called "deletedup"
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
    
    (setq B (ssget "X"))
    (princ (- (sslength A) (sslength B))) (princ "Duplicates Removed")
    (princ)
)


(defun deletedup (LAYCUR)
  (if (tblsearch "layer" LAYCUR)                                           ;if current layer doesn't exist, skip all of the subfunction "deletedup"
    (progn                                                                 ;everything inside of progn parentheses is "then", otherwise everything in first line would be "else"
      (command "LAYER" "SET" LAYCUR "OFF" "*" "Y" "ON" LAYCUR "")          ;isolates current search layer (LAYISO requires manual input, so not used)
      (command "_mapclean" "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\DUPL.dpf")                                   ;if mapclean finds errors, they are deleted
      (command "LAYER" "ON" "*" ""))))


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