(defun C:bdum ()
  (ssget "X" '((0 . "LWPOLYLINE") (8 . "B-OVERHANG")))
)