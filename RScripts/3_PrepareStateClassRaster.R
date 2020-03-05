#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 3. Prepare State Class Raster

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a PrepareStateClass GRASS mapset                                                                              #
# 2. Imports 1990 LANDCOVER raster, VRI, MPB, and CUTBLOCKS data to GRASS mapset                                           #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. 
############################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(tidyverse)
library(stringr)
library(openxlsx)

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
MPB_name <- paste0(spatialDataDir, "MPB/MPB.shp")
CUTBLOCKS_name <- paste0(spatialDataDir, "CUTBLOCKS/CUTBLOCKS.shp")

# Tabular data - Load
stateClassID <- read.xlsx(paste0(tabularDataDir,'StateClassID.xlsx'))

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
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='PrepareSecondaryStratum')

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "PrepareStateClass", flags="c")

# Import data
      # From shapefiles
execGRASS("v.import", input=MPB_name, output='rawData_MPB', 'overwrite')
execGRASS("v.import", input=CUTBLOCKS_name, output='rawData_CUTBLOCKS', 'overwrite')

      # From GRASS mapsets
execGRASS('g.copy', raster=c('LC1990_agg_mask@PreparePrimaryStratum' ,'LC1990_agg_mask'))
execGRASS('g.copy', vector=c('rawData_VRI@PreparePrimaryStratum' ,'rawData_VRI'))
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

#### Produce State Class raster ####


# Export
execGRASS('r.out.gdal', input='stateClass', output=paste0(resultsDir, 'Spatial/DataLayers/StateClass.tif'), 'overwrite')
