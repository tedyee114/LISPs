(defun C:bduzg ()
  (command "_layer" "_off" "G-TOPO-MINR" "")
  (command "_layer" "_off" "G-TOPO-MAJR" "")
  
  (setq A (ssget "X" '((0 . "LWPOLYLINE"))))              ;"A","B","C" are lists of acceptable entities
  (setq B (ssget "X" '((0 . "CIRCLE") (8 . "U-MANHOLE-TR,U-CATCHBASIN-TR,C-CONC-PAD"))))
  (setq C (ssget "X" '((0 . "INSERT") (8 . "U-MANHOLE,U-CATCHBASIN"))))
  
  ;Appender Loops
  (setq n (sslength B))                                   ;"n"=number of B
  (setq i -1)                                             ;"i"=-1 (0 at beginning of list)
  (repeat n (ssadd (ssname B (setq i (1+ i))) A))         ;appends ID of element #i in list B to list A
  
  (setq o (sslength C))                                   
  (setq j -1)                                             
  (repeat o (ssadd (ssname C (setq j (1+ j))) A))         

  (command "PSELECT" A "")                                ;isolates list A, which now includes B,C elements too
  (command "HIDEOBJECTS")
)