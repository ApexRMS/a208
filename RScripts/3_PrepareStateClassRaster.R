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
execGRASS('g.copy', raster=c('LC2010_agg_mask@PreparePrimaryStratum' ,'LC2010_agg_mask'))
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
execGRASS('g.region', raster='MASK', res=as.character(res))
execGRASS('g.region', zoom='MASK', res=as.character(res))

#### Input 1 - Get broad Land Cover from AAFC 2010 LANDCOVER layer ####
write.table(c('21=2', '25=2', '31=3', '41=4', '42=4', '45=4', '46=4', '51=5', '61=6', '61=6', '71=7', '73=7', '74=7', '91=9'), paste0(resultsDir, 'Tabular/Rules/StateClass_getBroadLandCover.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
execGRASS('r.reclass', input='LC2010_agg_mask', output='stateClass_broad_inter', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getBroadLandCover.txt'), flags=c('overwrite'))
execGRASS('r.mapcalc', expression='stateClass_broad = stateClass_broad_inter', region='current', flags='overwrite')
execGRASS('g.remove', type='raster', name='stateClass_broad_inter', flags='f')

#### Input 2 - Distinguish between Pine and Other Forest using VRI ####
# Add binary Pine/Other Forest column
      # Add column
execGRASS('v.db.addcolumn', map='rawData_VRI', layer='1', columns='Pine integer')

      # Create sqlite statement
rule <- paste('CASE WHEN (SPECIES_CD =', sQuote('PL'), 'OR SPECIES_CD =', sQuote('PLI'), ') AND (SPECIES_PC >= 25) THEN 1 ELSE 0 END')

      # Populate column
execGRASS('v.db.update', map='rawData_VRI', column='Pine', query_column=rule)

# Rasterize
execGRASS('v.to.rast', input='rawData_VRI', output='Pine_Other', use='attr', attribute_column='Pine', 'overwrite')

#### Input 3 - Identify areas that experienced MPB or clearcuts -- PICK UP HERE ####
# Identify areas that experienced MPB


# Identify areas that experienced clearcuts


#### Produce State Class raster ####
# Combine all 3 inputs to produce State Class raster

# Export
execGRASS('r.out.gdal', input='stateClass', output=paste0(resultsDir, 'Spatial/DataLayers/StateClass.tif'), 'overwrite')
