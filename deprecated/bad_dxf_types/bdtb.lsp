(defun C:bdtb ()
  (ssget "X" '((-4 . "/=") (0 . "LWPOLYLINE")))
)