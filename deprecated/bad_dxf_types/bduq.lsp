(defun C:bduq ()
  (ssget "X" '((0 . "CIRCLE")(-4 . "<OR")(8."U-MANHOLE-TR")(8."U-CATCHBASIN-TR")(8."C-CONC-PAD")))
)