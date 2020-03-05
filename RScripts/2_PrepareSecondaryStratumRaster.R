#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 2. Prepare Secondary Stratum Raster

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a PrepareSecondaryStratum GRASS mapset                                                                        #
# 2. Imports all land ownership vectors to GRASS mapset                                                                    #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. Rasterizes all land ownership vectors                                                                                 #
# 5. 
############################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(tidyverse)
library(stringr)

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
adminLands_name <- paste0(spatialDataDir, "BC_AdminLands/BC_AdminLands_Oct2019.shp")
ecologicalReserve_name <- paste0(spatialDataDir, "BC_EcologicalReserve/BC_EcologicalReserve_Oct2019.shp")
protectedArea_name <- paste0(spatialDataDir, "BC_ProtectedArea/BC_ProtectedArea_Oct2019.shp")
provPark_name <- paste0(spatialDataDir, "BC_ProvPark/BC_ProvPark_Oct2019.shp")
reserveLands_name <- paste0(spatialDataDir, "BC_ReserveLands/BC_ReserveLands_Oct2019.shp")
WHA_name <- paste0(spatialDataDir, "BC_WHA/BC_WHA_Oct2018.shp")
WMA_name <- paste0(spatialDataDir, "BC_WMA/BC_WMA_Oct2019.shp")

# Tabular data - Load

# Function - Get GRASS vector attribute table
v.get.att <- function(vector_name, sep){
  # Get attributes
  att <- execGRASS("v.db.select", map=vector_name, separator=sep, intern=T)
  
  # Format as dataframe
  tc <- textConnection(att)
  df <- read.table(tc, header = TRUE, sep=sep)
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
      # From shapefiles
execGRASS("v.import", input=landOwner_name, output='rawData_landOwner', 'overwrite')
execGRASS("v.import", input=adminLands_name, output='rawData_adminLands', 'overwrite')
execGRASS("v.import", input=ecologicalReserve_name, output='rawData_ecologicalReserve', 'overwrite')
execGRASS("v.import", input=protectedArea_name, output='rawData_protectedArea', 'overwrite')
execGRASS("v.import", input=provPark_name, output='rawData_provPark', 'overwrite')
execGRASS("v.import", input=reserveLands_name, output='rawData_reserveLands', 'overwrite')
execGRASS("v.import", input=WHA_name, output='rawData_WHA', 'overwrite')
execGRASS("v.import", input=WMA_name, output='rawData_WMA', 'overwrite')

      # From GRASS mapsets
execGRASS('g.copy', raster=c('MASK@PreparePrimaryStratum' ,'MASK'))

# Set mapset region
      # Get MASK resolution
            # Get row where resolution is displayed in r.info output
res_row <- execGRASS('r.info', map='MASK', intern=T) %>%
  str_locate_all(., 'Res:    ') %>%
  sapply(., nrow) %>%
  match(1, .)
  
            # Get starting column where resolution is displayed in r.info output
res_col <- execGRASS('r.info', map='MASK', intern=T)[res_row] %>%
  str_locate_all(., 'Res:    ') %>%
  .[[1]] %>%
  .[1,2] %>%
  as.integer(.) + 1
  
            # Get resolution
res <- execGRASS('r.info', map='MASK', intern=T)[res_row] %>%
  substr(., start=res_col, stop=res_col+10) %>%
  as.integer(.)
rm(res_row, res_col)

      # Apply to region
execGRASS('g.region', zoom='MASK', res=as.character(res))

#### Rasterize all input vectors ####
# Land owner
      # Create land ownership key
landOwner_att <- v.get.att(vector_name='rawData_landOwner', sep='&')


execGRASS('v.to.rast', input='rawData_landOwner', output='landOwner_mask', use='attr', attribute_column='OWNER_TYPE')
