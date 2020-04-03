#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 8. Habitat Suitability Analysis

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a HabitatSuitabilityAnalysis GRASS mapset                                                                           #
# 2. Imports Secondary Stratum and LULC data for 1990, 2000, and 2010 to GRASS mapset                                      #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
############################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(tidyverse)
library(magrittr)
library(openxlsx)
library(blme)

# Input parameters
cutImpactOnAdjacentCells <- 'None' # Must be one of "None", "Rook", or "Queen"

# Settings
Sys.setenv(TZ='GMT')
options(stringsAsFactors=FALSE, SHAPE_RESTORE_SHX=T, useFancyQuotes = F, digits=10)

# Directories
gisBase <- "C:/Program Files/GRASS GIS 7.4.3"
gisDbase <- "E:/a208/Results/Spatial/grass7"
spatialDataDir <- "E:/a208/Data/Spatial/"
tabularDataDir <- "E:/a208/Data/Tabular/"
resultsDir <- "E:/a208/Results/"

# Spatial Data - Names
stateClass_out_name <- paste0(resultsDir, "Spatial/SyncroSim Outputs/StateClass.tif")
forestAge_out_name <- paste0(resultsDir, "Spatial/SyncroSim Outputs/ForestAge.tif")

# Statistical Models - Load
m0 <- readRDS(paste0(tabularDataDir,"OSFL_uncut5.rds"))
m1 <- readRDS(paste0(tabularDataDir,"OSFL_cut4.rds"))

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
# Initiate GRASS in 'PrepareStateClass' mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='LandCoverChangeRates')

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "HabitatSuitabilityAnalysis", flags="c")

# Import data
execGRASS('r.import', input=stateClass_out_name, output='stateClass_out')
execGRASS('r.import', input=forestAge_out_name, output='forestAge_out')
execGRASS('g.copy', raster=c('MASK@PreparePrimaryStratum', 'MASK'))

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
execGRASS('g.region', raster='MASK', res=as.character(res))
execGRASS('g.region', zoom='MASK', res=as.character(res))

#### Habitat Suitability Analysis ####
