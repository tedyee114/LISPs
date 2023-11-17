(defun C:bduy ()
  (ssget "X" '((-4 . "<OR")(0 . "CIRCLE")(0 . "LINE")  (-4 . "OR>")))
)