(defun C:bduo ()
  (ssget "X" '((0 . "LWPOLYLINE") (8 . "B-OVERHANG, R-ROAD-ASPH")))
)