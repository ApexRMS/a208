#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 2. Prepare Secondary Stratum Raster

############################################################################################################################
# This code:                                                                                                               #
# 1. 
############################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(tidyverse)

# Settings
Sys.setenv(TZ='GMT')
options(stringsAsFactors=FALSE, SHAPE_RESTORE_SHX=T, useFancyQuotes = F, digits=10)

# Directories
gisBase <- "C:/Program Files/GRASS GIS 7.4.3"
gisDbase <- "E:/a208/Results/Spatial/grass7"
spatialDataDir <- "E:/a208/Data/Spatial/"
tabularDataDir <- "E:/a208/Data/Tabular/"
resultsDir <- "E:/a208/Results/"

# Input Parameters
canopyCoverThreshold <- 50 # Canopy cover value starting at which the canopy will be considered "closed" (otherwise considered "open"), in %

# Spatial data - Names
landOwner_name <- paste0(spatialDataDir, "Land_Owner/Land_Owner.shp")
adminLands_name <- paste0(spatialDataDir, "BC_AdminLands/BC_AdminLands.shp")
ecologicalReserve_name <- paste0(spatialDataDir, "BC_EcologicalReserve/BC_EcologicalReserve.shp")
protectedArea_name <- paste0(spatialDataDir, "BC_ProtectedArea/BC_ProtectedArea.shp")
provPark_name <- paste0(spatialDataDir, "BC_ProvPark/BC_ProvPark.shp")
reserveLands_name <- paste0(spatialDataDir, "BC_ReserveLands/BC_ReserveLands.shp")
WHA_name <- paste0(spatialDataDir, "BC_WHA/BC_WHA.shp")
WMA_name <- paste0(spatialDataDir, "BC_WHA/BC_WMA.shp")

# Tabular data - Load

# Function - Get GRASS vector attribute table
v.get.att <- function(vector_name){
  # Get attributes
  att <- execGRASS("v.db.select", map=vector_name, separator=':', intern=T)
  
  # Format as dataframe
  tc <- textConnection(att)
  df <- read.table(tc, header = TRUE, sep=':')
  close(tc)
  
  # Return resulting dataframe
  return(df)
}

#### Set up GRASS Mapset #####
# Initiate GRASS in 'PreparePrimaryStratum' mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='PreparePrimaryStratum')

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "PrepareSecondaryStratum", flags="c")

# Import data
execGRASS("v.import", input=landOwner_name, output='rawData_landOwner')
execGRASS("v.import", input=adminLands_name, output='rawData_landOwner')
execGRASS("v.import", input=ecologicalReserve_name, output='rawData_landOwner')
execGRASS("v.import", input=protectedArea_name, output='rawData_landOwner')
execGRASS("v.import", input=provPark_name, output='rawData_landOwner')
execGRASS("v.import", input=reserveLands_name, output='rawData_landOwner')
execGRASS("v.import", input=WHA_name, output='rawData_landOwner')
execGRASS("v.import", input=WMA_name, output='rawData_landOwner')
