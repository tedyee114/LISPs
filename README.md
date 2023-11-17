# LISPs
AutoLISP add-ons for faster CAD work
Created by Ted Yee 2023 for open use at Airworks.io
Designed for company procedures specifically, but modular pieces are adaptable for almost all AutoCAD use


;THWOMP, UC, NB, GER, BDT, DU, SWAG by Ted Yee 9/18/23, VERTNUM by unknown, SAYIT by Terry CADD 01/18/2007, BUFF by Kent1Cooper Autodesk Forums, adapted by Ted Yee 9/15/2023, AVX, DVX  by Gilles Chanteau 12/05/07
;
;Active layer cannot be a non-training layer, otherwise there is an interrupt error
;There were many versions of this, comments may not have been updated. If comment looks verywrong, it probably is outdated
;
;User Commands:
;SAYIT-   LINE 033- reads out loud inputted text. For fun, lol
;VERTNUM- LINE 043- prints number of vertices of selected polyline (sometimes off by 1)
;BUFF-    LINE 055- breates buffer around polylines with entered values being full cross-width, options for endcaps and smooth corners
;THWOMP-  LINE 129- hides company non-training layers, then isolates polylines with <=2nodes
;UC-      LINE 148- same as QSELECT>POLYLINE>CLOSED>NO, isolates unclosed polylines on training layers
;NB-      LINE 166- makes every entity's color "By-Layer", displays number of fixes made
;GER-     LINE 173- *requires GER.dpf* runs MAPCLEAN with GER.dpf= MAPCLEAN>INTERACTIVE>BREAK CROSSING OBJECTS>FINISH (these saved as GER.pf), locates geos-r objects
;BDT-     LINE 180- isolates "bad dxf-types" (unacceptable entities at company: anything that is not a polyline, a circle, or circular U-MANHOLE-TR)
;DU-      LINE 224- *very calculation heavy* isolates duplicate objects
;SWAG-    LINE 208- *requires GEOS-R-TEMP.dpf & DUPL.dpf* file checker like company used to use on swagger.io, checks for 2node polys, uncloseds, bad-dxf-types, geos-r errors, and fixes non-by-layer-colored and duplicate entities.
;AVX-     LINE 467- Adds Vertex on polyline (LW, 2D, 3D) 
;DVX-     LINE 761- Deletes Vertex on polyline (LW, 2D, 3D)

;;Optional manual input of drawing cleanup file locations: (must use double slash \\, otherwise invalid)
;(setq GER_LOCATION         "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\GER.dpf")          ;GER.dpf is same as MAPCLEAN>BREAK CROSSING OBJECTS>INTERACTIVE
;(setq DUPL_LOCATION        "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\DUPL.dpf")         ;DUPL.dpf is same as MAPCLEAN>DELETE DUPLICATES>AUTOMATIC
;(setq GEOS-R-TEMP_LOCATION "C:\\Users\\ted_airworks.io\\Documents\\Scripts\\LISPs\\GEOS-R-TEMP.dpf")  ;GEOS-R-TEMP.dpf is same as MAPCLEAN>BREAK CROSSING OBJECTS>AUTOMATIC, RETAIN ORIGINALS AND CREATE NEW OBJECTS ON LAYER "GEOS-R-TEMP"
