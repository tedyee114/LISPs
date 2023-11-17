(defun C:bduw ()
  (ssget "X" '((-4 . "<OR")(0 . "CIRCLE")(0 . "LWPOLYINE")  (-4 . "OR>")))
)