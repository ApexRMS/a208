#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 1. Data Pre-Processing

###########################################################################################################################
# This code:                                                                                                              #
# - 
###########################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)

# Settings
Sys.setenv(TZ='GMT')
options(stringsAsFactors=FALSE)

# Directories
gisBase <- "C:/OSGeo4W64/apps/grass/grass78"
gisDbase <- "E:/a208/Results/Spatial/grass7"
spatialDataDir <- "E:/a208/Data/Spatial/"
tabularDataDir <- "E:/a208/Data/Tabular/"

# Parameters
canopyCoverThreshold <- 50 # Canopy cover value starting at which the canopy will be considered "closed" (otherwise considered "open"), in %

# Spatial data - Names
#VRI <- paste0(spatialDataDir, )

#### Set up GRASS Location and GRASS Mapset #####
# Create location and PERMANENT mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='PERMANENT', override=TRUE)

# Set projection info
execGRASS("g.proj", georef=SE_1996_Name, flags="c")

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "DataPreProcessing", flags="c")

# Import data that is in the corresponding CRS
