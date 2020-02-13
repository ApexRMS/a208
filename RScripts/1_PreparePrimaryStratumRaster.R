#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 1. Prepare Primary Stratum Raster

###########################################################################################################################
# This code:                                                                                                              #
# - Creates a GRASS Location and Mapset for a208                                                                          #
# - Loads VRI vector data into the mapset                                                                                 #
# - Sets the mapset region to that of the Dawson Creek TSA and the mapset resolution to 100m                              # 
# - Computes a unique BEC Subszone ID for each VRI feature [UNFINISHED: would be best with standalone GRASS installation] #                                                              #                                                                                     #
# - Creates a raster containing the resulting ID, with resolution = 100m                                                  #
###########################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(openxlsx)

# Settings
Sys.setenv(TZ='GMT')
options(stringsAsFactors=FALSE, SHAPE_RESTORE_SHX=T)

# Directories
gisBase <- "C:/OSGeo4W64/apps/grass/grass78"
gisDbase <- "E:/a208/Results/Spatial/grass7"
spatialDataDir <- "E:/a208/Data/Spatial/"
tabularDataDir <- "E:/a208/Data/Tabular/"

# Parameters
canopyCoverThreshold <- 50 # Canopy cover value starting at which the canopy will be considered "closed" (otherwise considered "open"), in %

# Spatial data - Names
VRI_name <- paste0(spatialDataDir, "VRI/VRI_DawsonCreekTSA.shp")
# adminLands_name <- paste0(spatialDataDir, "BC_AdminLands/BC_AdminLands_Oct2019.shp")
# ecologicalReserve_name <- paste0(spatialDataDir, "BC_EcologicalReserve/BC_EcologicalReserve_Oct2019.shp")
# protectedArea_name <- paste0(spatialDataDir, "BC_ProtectedArea/BC_ProtectedArea_Oct2019.shp")
# provPark_name <- paste0(spatialDataDir, "BC_ProvPark/BC_ProvPark_Oct2019.shp")
# reserveLands_name <- paste0(spatialDataDir, "BC_ReserveLands/BC_ReserveLands_Oct2019.shp")
# WHA_name <- paste0(spatialDataDir, "BC_WHA/BC_WHA_Oct2018.shp")

# Tabular data - Load
subzoneID <- read.xlsx(paste0(tabularDataDir, "BEC Subzone.xlsx"))

#### Set up GRASS Location and GRASS Mapset #####
# Create location and PERMANENT mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='PERMANENT', override=TRUE)

# Set projection info
execGRASS("g.proj", georef=VRI_name, flags="c")

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "DataPreProcessing", flags="c")

# Import data
execGRASS("v.in.ogr", input=VRI_name, output='rawData_VRI', snap=1e-005)
# execGRASS("v.in.ogr", input=adminLands_name, output='rawData_adminLands')
# execGRASS("v.in.ogr", input=ecologicalReserve_name, output='rawData_ecologicalReserve') # Missing .shx file
# execGRASS("v.in.ogr", input=protectedArea_name, output='rawData_protectedArea')
# execGRASS("v.in.ogr", input=provPark_name, output='rawData_provPark')
# execGRASS("v.in.ogr", input=reserveLands_name, output='rawData_reserveLands')
# execGRASS("v.in.ogr", input=WHA_name, output='rawData_WHA') # Missing .shx file

# Set mapset region
execGRASS('g.region', n='1266750', s='1026050', w='1188850', e='1390150')

#### Add unique ID column ####
# Compute IDs
      # Get BEC_ZONE_C

      # Get BEC_SUBZONE


# Populate attribute table with IDs
execGRASS('v.db.addcolumn', map='rawData_VRI', layer='1', columns='ID integer')
execGRASS('v.db.update', map='myfields', layer='1', column='ID', value=IDs)

#### Rasterize ####
execGRASS('v.to.rast', input='rawData_VRI', output='primaryStratum', use='ID')
