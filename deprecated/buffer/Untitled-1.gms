///Ted Yee 8-23-2023 for AirWorks Inc.
///Files must be loaded as Layer 0=KML, 1=Pointcloud, (if buildings or water exist, they can be 2 or 3), not other layers
///For Tiled Pointclouds, put all tiles on one layer
GLOBAL_MAPPER_SCRIPT

// Loop over all files in folder #1
DIR_LOOP_START DIRECTORY="C:\Users\AirWorksProcessing\Downloads\1828\" FILENAME_MASKS= "*.tif" RECURSE_DIR=YES
  //Import each file
  IMPORT FILENAME="%FNAME_W_DIR%"
DIR_LOOP_END
