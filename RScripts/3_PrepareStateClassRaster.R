#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 3. Prepare State Class Raster

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a PrepareStateClass GRASS mapset                                                                              #
# 2. Imports 1990 LANDCOVER raster, VRI, MPB, and CUTBLOCKS data to GRASS mapset                                           #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. Produces a raster of broad Land Cover from AAFC 2010 LANDCOVER layer (Input 1)                                        #
# 5. Produces a raster of Pine vs. Other Forest from the VRI (Input 2)                                                     #
# 6. Produces a raster of MPB-impacted areas (Input 3)                                                                     #
# 7. Produces a raster of clearcut areas (Input 4)                                                                         #
# 8. Combines all four inputs to produce the State Class raster                                                            #
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
execGRASS('r.reclass', input='LC2010_agg_mask', output='LC2010_broad_mask_inter', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getBroadLandCover.txt'), flags=c('overwrite'))
execGRASS('r.mapcalc', expression='LC2010_broad_mask = LC2010_broad_mask_inter', region='current', flags='overwrite')
execGRASS('g.remove', type='raster', name='LC2010_broad_mask_inter', flags='f')

#### Input 2 - Distinguish between Pine and Other Forest using VRI ####
# Add binary Pine/Other Forest column
      # Add column
execGRASS('v.db.addcolumn', map='rawData_VRI', layer='1', columns='Pine integer')

      # Create sqlite statement
rule <- paste('CASE WHEN (SPECIES_CD =', sQuote('PL'), 'OR SPECIES_CD =', sQuote('PLI'), ') AND (SPECIES_PC >= 25) THEN 1 ELSE 0 END')

      # Populate column
execGRASS('v.db.update', map='rawData_VRI', column='Pine', query_column=rule)

# Rasterize
execGRASS('v.to.rast', input='rawData_VRI', output='Pine_mask', use='attr', attribute_column='Pine', 'overwrite')

#### Input 3 - Identify areas that experienced MPB ####
# Get areas with Moderate, Severe, or Very Severe MPB
execGRASS('v.extract', input='rawData_MPB', where="(PST_SEV_CD = 'M') OR (PST_SEV_CD = 'S') OR (PST_SEV_CD = 'V')", output='MPBimpacted')

# Rasterize
execGRASS('v.to.rast', input='MPBimpacted', output='MPB_mask_inter', use='attr', attribute_column='cat', 'overwrite')
execGRASS('r.mapcalc', expression='MPB_mask = if(MPB_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='MPB_mask_inter', 'f')

#### Input 4 - Identify areas that experienced clearcuts ####
execGRASS('v.to.rast', input='rawData_CUTBLOCKS', output='CUT_mask_inter', use='attr', attribute_column='cat', 'overwrite')
execGRASS('r.mapcalc', expression='CUT_mask = if(CUT_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='CUT_mask_inter', 'f')

#### Produce State Class raster ####
# Produce forest classification
execGRASS('r.mapcalc', expression='Forest_mask = if(LC2010_broad_mask == 4, if(Pine_mask == 1, if(not(isnull(CUT_mask)), 12, if(not(isnull(MPB_mask)), 11, 10)), if(not(isnull(CUT_mask)), 14, 13)), null())', 'overwrite')

# Add to broad Land Cover classification
execGRASS('r.patch', input=c('Forest_Mask', 'LC2010_broad_mask'), output='stateClass')

# Add category labels
write.table(paste(stateClassID$State.Class.ID, stateClassID$State.Class.Name, sep=";"), paste0(resultsDir, 'Tabular/Rules/StateClass_getLabels.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
execGRASS('r.category', map='stateClass', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getLabels.txt'), separator=";")

# Export
execGRASS('r.out.gdal', input='stateClass', output=paste0(resultsDir, 'Spatial/DataLayers/StateClass.tif'), 'overwrite')
