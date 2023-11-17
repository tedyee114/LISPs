(defun C:bduk ()
  (setq polys (ssget "X" '((0 . "LWPOLYLINE")(8. B-OVERHANG))))
)