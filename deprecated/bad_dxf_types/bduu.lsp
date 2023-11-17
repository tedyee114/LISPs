(defun C:bduu ()
  (ssget "X" '((0 . "CIRCLE")(8."U-MANHOLE-TR, U-CATCHBASIN-TR, C-CONC-PAD")))
)