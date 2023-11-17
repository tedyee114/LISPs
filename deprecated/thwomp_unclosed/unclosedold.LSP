(
defun c:unclosedold ()
   (ssget "X" '((0 . "LWPOLYLINE") (70 . 1)))
)