#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 2. Prepare Secondary Stratum Raster

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a PrepareSecondaryStratum GRASS mapset                                                                        #
# 2. Imports all land ownership vectors to GRASS mapset                                                                    #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. Rasterizes all land ownership vectors                                                                                 #
# 5. Produces the Secondary Stratum raster from land ownership rasters                                                     #
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
landOwner_name <- paste0(spatialDataDir, "Land_Owner/Land_Owner.shp")
adminLands_name <- paste0(spatialDataDir, "BC_AdminLands/BC_AdminLands_Oct2019.shp")
ecologicalReserve_name <- paste0(spatialDataDir, "BC_EcologicalReserve/BC_EcologicalReserve_Oct2019.shp")
protectedArea_name <- paste0(spatialDataDir, "BC_ProtectedArea/BC_ProtectedArea_Oct2019.shp")
provPark_name <- paste0(spatialDataDir, "BC_ProvPark/BC_ProvPark_Oct2019.shp")
reserveLands_name <- paste0(spatialDataDir, "BC_ReserveLands/BC_ReserveLands_Oct2019.shp")
WHA_name <- paste0(spatialDataDir, "BC_WHA/BC_WHA_Oct2018.shp")
WMA_name <- paste0(spatialDataDir, "BC_WMA/BC_WMA_Oct2019.shp")

# Tabular data - Load
secondaryStratumID <- read.xlsx(paste0(tabularDataDir, 'SecondaryStratumID.xlsx'))

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
execGRASS('g.region', raster='MASK', res=as.character(res))
execGRASS('g.region', zoom='MASK', res=as.character(res))

#### Rasterize all input vectors ####
# Land owner
      # Create land ownership key
landOwner_att <- v.get.att(vector_name='rawData_landOwner', sep='&')
landOwner_key <- data.frame(Code = 1:length(unique(landOwner_att$OWNER_TYPE)),
                            Label = sort(unique(landOwner_att$OWNER_TYPE)))

      # Add land ownership key to attribute table
            # Add column 
execGRASS('v.db.addcolumn', map='rawData_landOwner', layer='1', columns='Code integer')

            # Create sqlite statement
rule <- paste('CASE', paste(paste('WHEN OWNER_TYPE =', sQuote(landOwner_key$Label), 'THEN', landOwner_key$Code), collapse=" "), 'END')

            # Populate column
execGRASS('v.db.update', map='rawData_landOwner', column='Code', query_column=rule)

      # Rasterize on land ownership key
execGRASS('v.to.rast', input='rawData_landOwner', output='landOwner_mask', use='attr', attribute_column='Code', label_column='OWNER_TYPE')

# Admin Lands
      # Rasterize
execGRASS('v.to.rast', input='rawData_adminLands', output='adminLands_mask_inter', use='attr', attribute_column='cat')

      # Set all cells to 1
execGRASS('r.mapcalc', expression='adminLands_mask = if(adminLands_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='adminLands_mask_inter', 'f')

# Ecological Reserve
      # Rasterize
execGRASS('v.to.rast', input='rawData_ecologicalReserve', output='ecologicalReserve_mask_inter', use='attr', attribute_column='cat')

      # Set all cells to 1
execGRASS('r.mapcalc', expression='ecologicalReserve_mask = if(ecologicalReserve_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='ecologicalReserve_mask_inter', 'f')

# Protected Area
      # Rasterize
execGRASS('v.to.rast', input='rawData_protectedArea', output='protectedArea_mask_inter', use='attr', attribute_column='cat')

      # Set all cells to 1
execGRASS('r.mapcalc', expression='protectedArea_mask = if(protectedArea_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='protectedArea_mask_inter', 'f')

# Prov Park
      # Rasterize
execGRASS('v.to.rast', input='rawData_provPark', output='provPark_mask_inter', use='attr', attribute_column='cat')

      # Set all cells to 1
execGRASS('r.mapcalc', expression='provPark_mask = if(provPark_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='provPark_mask_inter', 'f')

# Reserve Lands
      # Rasterize
execGRASS('v.to.rast', input='rawData_reserveLands', output='reserveLands_mask_inter', use='attr', attribute_column='cat')

      # Set all cells to 1
execGRASS('r.mapcalc', expression='reserveLands_mask = if(reserveLands_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='reserveLands_mask_inter', 'f')

# WHA
      # Rasterize
execGRASS('v.to.rast', input='rawData_WHA', output='WHA_mask_inter', use='attr', attribute_column='cat')

      # Set all cells to 1
execGRASS('r.mapcalc', expression='WHA_mask = if(WHA_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='WHA_mask_inter', 'f')

# WMA
      # Rasterize
execGRASS('v.to.rast', input='rawData_WMA', output='WMA_mask_inter', use='attr', attribute_column='cat')

      # Set all cells to 1
execGRASS('r.mapcalc', expression='WMA_mask = if(WMA_mask_inter)', 'overwrite')
execGRASS('g.remove', type='raster', name='WMA_mask_inter', 'f')

#### Produce Secondary Stratum raster ####
# Create raster with Secondary Stratum ID = 1
execGRASS('r.patch', input=c('adminLands_mask', 'ecologicalReserve_mask', 'protectedArea_mask', 'provPark_mask', 'reserveLands_mask', 'WHA_mask', 'WMA_mask'), output='secondaryStratum1')

# Create raster with Secondary Stratum ID = 2
write.table(c('8=2', '6=2', '4=2', '*=NULL'), paste0(resultsDir, 'Tabular/Rules/LandOwner_getPrivate.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
execGRASS('r.reclass', input='landOwner_mask', output='secondaryStratum2_inter', rules=paste0(resultsDir, 'Tabular/Rules/LandOwner_getPrivate.txt'))
execGRASS('r.mapcalc', expression='secondaryStratum2 = secondaryStratum2_inter', 'overwrite')
execGRASS('g.remove', type='raster', name='secondaryStratum2_inter', 'f')

# Create raster with Secondary Stratum ID = 3
execGRASS('r.mapcalc', expression='secondaryStratum3 = if(MASK, 3)')

# Compile into secondary stratum raster
execGRASS('r.patch', input=c('secondaryStratum1', 'secondaryStratum2', 'secondaryStratum3'), output='secondaryStratum', 'overwrite')

# Add category labels
write.table(paste(secondaryStratumID$Secondary.Stratum.ID, secondaryStratumID$Name, sep=":"), paste0(resultsDir, 'Tabular/Rules/SecondaryStratum_getLabels.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
execGRASS('r.category', map='secondaryStratum', rules=paste0(resultsDir, 'Tabular/Rules/SecondaryStratum_getLabels.txt'), separator=":")
execGRASS('r.colors', map='secondaryStratum', 'r')

# Export
execGRASS('r.out.gdal', input='secondaryStratum', output=paste0(resultsDir, 'Spatial/DataLayers/SecondaryStratum.tif'), 'overwrite')
