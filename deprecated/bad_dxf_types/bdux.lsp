(defun C:bdux ()
  (ssget "X" '((-4 . "<OR")(0 . "CIRCLE")(0 . "POLYINE")  (-4 . "OR>")))
)