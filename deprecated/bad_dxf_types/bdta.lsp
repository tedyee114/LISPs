(defun C:bdta ()
  (ssget "X" '((-4 . "<=") (0 . "LWPOLYLINE")))
)