(defun C:foozzx ()
  (setq ERRORLIST (list Hi))
  (setq LAYCUR "B-OVERHANG")  (cleaner)
  (setq LAYCUR "R-ROAD-ASPH") (cleaner)

  ;; (print "Errors on:      ") (princ (length ERRORLIST))
  ;;   (setq n (length ERRORLIST))                               ;"n"=number of duplicates
  ;;   (setq i -1)                                          ;"i"=-1 (0 at beginning of list)
  ;;   (repeat n
  ;;     (setq i (1+ i))                       ;"CURRENT"=name of entity currently being evaluated
  ;;     (princ " ")(princ (+ 1 i))(princ ".")
  ;;     (princ (setq blah i !ERRORLIST)))           ;searches within the entity info for value type 8 (layer)  (princ)
)

(defun cleaner ()
  (if (not (tblsearch "layer" "GEOS-R-TEMP")) 
    (command "_layer" "m" "GEOS-R-TEMP" "")
  )
  (command "_layer" "set" LAYCUR "")
  (command "_layer" "_off" "*" "N" "")
  (command "_layer" "_on" LAYCUR "_on" "GEOS-R-TEMP" "")
  (command "_mapclean" "GEOS-R-TEMP")
  (princ (setq num (sslength (setq new (ssget "_A" '((8 . "GEOS-R-TEMP")))))))
  (if (/= num 0)
   (append ERRORLIST '(LAYCUR))
  )
  (command "_laydel" "n" "GEOS-R-TEMP" "" "Y" "")
  (command "_layer" "_on" "*" "")
 )
