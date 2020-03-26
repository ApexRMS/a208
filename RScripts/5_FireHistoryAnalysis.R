#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 5. Fire History Analysis

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a FireHistoryAnalysis GRASS mapset                                                                            #
# 2. Imports Primary Stratum data and Fire Perimeters data to GRASS mapset                                                 #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
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

# Spatial data - Names
FirePerimeters_name <- paste0(resultsDir, "Spatial/DataLayers/FirePerimeters_WGS84_UTM10N.shp")

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
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='PrepareForestAge')

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "FireHistoryAnalysis", flags="c")

# Import data
execGRASS('g.copy', raster=c('primaryStratum@PreparePrimaryStratum', 'primaryStratum'))
execGRASS("v.import", input=FirePerimeters_name, output='rawData_FirePerimeters', 'o')
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
