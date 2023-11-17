(defun C:bdul ()
  (setq polys (ssget "X" '((0 . "LWPOLYLINE")(8. "B-OVERHANG"))))
)