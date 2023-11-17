(defun c:swag ()
   ;;;duplicate
   long as code snippet here
   

   ;;;unclosed
   (ssget "X" '((0 . "LWPOLYLINE") (70 . 0)))

   ;;;non-by-layer
  (print "Number of Non-By-Layer objects fixed:")
  (command "_SetByLayer" allobjects "" "Yes" "Yes")


   ;;;bad-dxf-type- except conc, manholes, catch
iterate types, check for objects


   ;;;2node-        except non-training
   (ssget "X" '((0 . "LWPOLYLINE") (-4 . "<=") (90 . 2)))
)