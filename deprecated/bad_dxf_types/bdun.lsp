(defun C:bdun ()
  (ssget "X" '((0 . "LWPOLYLINE") (8 . "B-OVERHANG, R-PAVEMARK-LINE")))
)