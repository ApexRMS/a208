#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 1. Prepare Primary Stratum Raster

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a GRASS Location and Mapset for a208                                                                          #
# 2. Loads AAFC LANDCOVER and VRI data into the mapset                                                                     #
# 3. Sets the mapset region resolution to value of input parameter 'res'                                                   # 
# 4. Aggregates the AAFC LANDCOVER data to resolution = 'res'                                                              #
# 5. Computes a unique BEC Subszone ID for each VRI feature                                                                #                                                              #                                                                                     #
# 6. Creates a raster containing the resulting ID, with resolution = 'res'                                                 #
# 7. Creates and applies to all layers a clipping mask corresponding to the footprint of the raster obtained in 6.         #
# 8. Produces the Primary Stratum raster by adding wetlands to the raster produced in 6.                                   #
############################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(openxlsx)
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
res <- 90 # Resolution in meters
if(!res%%30==0){stop("Selected resolution is not a multiple of 30")} # Check that the selected resolution is a multiple of 30 (the resolution of the LANDCOVER data)

# Spatial data - Names
VRI_name <- paste0(spatialDataDir, "VRI/VRI_DawsonCreekTSA.shp")
LC1990_name <- paste0(spatialDataDir, "Land_Cover/IMG_AAFC_LANDUSE_Z10_1990/IMG_AAFC_LANDUSE_Z10_1990.tif")
LC2000_name <- paste0(spatialDataDir, "Land_Cover/IMG_AAFC_LANDUSE_Z10_2000/IMG_AAFC_LANDUSE_Z10_2000.tif")
LC2010_name <- paste0(spatialDataDir, "Land_Cover/IMG_AAFC_LANDUSE_Z10_2010/IMG_AAFC_LANDUSE_Z10_2010.tif")

# Tabular data - Load
subzoneID <- read.xlsx(paste0(tabularDataDir, "BEC Subzone.xlsx"), sheet="Stratum") %>% # Load
  mutate(Name = ifelse(Name == 'SBSwc', 'SBSwk', ifelse(Name == 'ESSFmv2', 'ESSFmv', ifelse(Name == 'BWBSwk1', 'BWBSwk', Name)))) # Correct errors in .xlsx file

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

#### Set up GRASS Location and GRASS Mapset #####
# Create location and PERMANENT mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='PERMANENT', override=TRUE)

# Set projection info
execGRASS("g.proj", georef=LC1990_name, flags="c")

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "PreparePrimaryStratum", flags="c")

# Import data
execGRASS("r.import", input=LC1990_name, output='rawData_LC1990', 'overwrite')
execGRASS("r.import", input=LC2000_name, output='rawData_LC2000', 'overwrite')
execGRASS("r.import", input=LC2010_name, output='rawData_LC2010', 'overwrite')
execGRASS("v.import", input=VRI_name, output='rawData_VRI', snap=1e-005)

# Set mapset region to the extent of the LANDCOVER data, with resolution = res
      # Set to minimum region encompassing LANDCOVER
execGRASS('g.region', zoom='rawData_LC1990')

      # Get number of rows and columns in region
nRows <- execGRASS('g.region', 'p', intern=T)[11] %>%
  gsub('rows:       ', '', .) %>%
  as.numeric()
nCols <- execGRASS('g.region', 'p', intern=T)[12] %>%
  gsub('cols:       ', '', .) %>%
  as.numeric()

      # Get northern and eastern limits of region
nBound <- execGRASS('g.region', 'p', intern=T)[5] %>%
  gsub('north:      ', '', .) %>%
  as.numeric()
eBound <- execGRASS('g.region', 'p', intern=T)[8] %>%
  gsub('east:       ', '', .) %>%
  as.numeric()

      # Adjust northern/eastern limits if nRows/nCols is not a multiple of res
if(!nRows%%(res/30)==0){ # If nRows not divisible by the ratio of res to 30m
  nBound <- nBound + res - 30*nRows%%(res/30) # Adjust northern limit
}

if(!nCols%%(res/30)==0){ # If nCols not divisible by the ratio of res to 30m
  eBound <- eBound + res - 30*nCols%%(res/30) # Adjust eastern limit
}

      # Apply new northern and eastern limits
execGRASS('g.region', n=as.character(nBound), e=as.character(eBound))

      # Set desired resolution
execGRASS('g.region', res=as.character(res))

# Aggregate LANDCOVER data to desired resolution
execGRASS('r.resamp.stats', input='rawData_LC1990', output='LC1990_agg', method='mode', 'overwrite')
execGRASS('r.resamp.stats', input='rawData_LC2000', output='LC2000_agg', method='mode', 'overwrite')
execGRASS('r.resamp.stats', input='rawData_LC2010', output='LC2010_agg', method='mode', 'overwrite')

#### Format VRI - Add unique ID column ####
# Stratum Name
      # Add column 
execGRASS('v.db.addcolumn', map='rawData_VRI', layer='1', columns='Name VARCHAR(15)')

      # Populate column
execGRASS('db.execute', sql="UPDATE rawData_VRI SET Name=BEC_ZONE_C || BEC_SUBZON")

# Stratum ID
      # Add column
execGRASS('v.db.addcolumn', map='rawData_VRI', layer='1', columns='ID integer')

      # Create sqlite statement
rule <- paste('CASE', paste(paste('WHEN Name =', sQuote(subzoneID$Name), 'THEN', subzoneID$ID), collapse=" "), 'END')

      # Populate column
execGRASS('v.db.update', map='rawData_VRI', column='ID', query_column=rule)

#### Format VRI - Rasterize ####
execGRASS('v.to.rast', input='rawData_VRI', output='BEC_ID', use='attr', attribute_column='ID', label_column='Name', 'overwrite')

#### Apply Clipping Mask  ####
# Set region to that of VRI
execGRASS('g.region', zoom='BEC_ID')

# Produce clipping mask
execGRASS('r.mask', raster='BEC_ID', 'overwrite')

# Clip all layers to clipping mask
execGRASS('r.mapcalc', expression='LC1990_agg_mask = LC1990_agg')
execGRASS('r.mapcalc', expression='LC2000_agg_mask = LC2000_agg')
execGRASS('r.mapcalc', expression='LC2010_agg_mask = LC2010_agg')

#### Produce Primary Stratum raster ####
# Produce layer of wetlands from 2010 LANDCOVER data
write.table(c('71=10', '73=10', '74=10', '*=NULL'), paste0(resultsDir, 'Tabular/Rules/LANDCOVER_getWetlands.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
execGRASS('r.reclass', input='LC2010_agg_mask', output='LC2010_wetlands_inter', rules=paste0(resultsDir, 'Tabular/Rules/LANDCOVER_getWetlands.txt'), flags=c('overwrite'))
execGRASS('r.mapcalc', expression='LC2010_wetlands_mask = LC2010_wetlands_inter', region='current', flags='overwrite')
execGRASS('g.remove', type='raster', name='LC2010_wetlands_inter', flags='f')

# Overlay wetlands on the BEC_ID raster
execGRASS('r.patch', input=c('LC2010_wetlands_mask', 'BEC_ID'), output='primaryStratum', 'overwrite')

# Export
execGRASS('r.out.gdal', input='primaryStratum', output=paste0(resultsDir, 'Spatial/DataLayers/PrimaryStratum.tif'))
