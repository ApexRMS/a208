#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chlo? Debyser

#### 8. Habitat Suitability Analysis

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a HabitatSuitabilityAnalysis GRASS mapset                                                                     #
# 2. Imports SyncroSim State Class and Forest Age output rasters to GRASS mapset                                           #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
############################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(tidyverse)
library(magrittr)
library(openxlsx)
library(rsyncrosim)
library(raster)
library(blme)

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
scenarioIds <- c(27) # Can be one or multiple Scenario IDs
library_path <- paste0(resultsDir, "ssimLibrary/DawsonTSA.ssim.backup.2020-04-09-at-16-04-24/DawsonTSA.ssim")
  
# Statistical Models
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
# Remove any pre-existing HabitatSuitabilityAnalysis GRASS mapset
unlink(paste0(gisDbase, "/a208/HabitatSuitabilityAnalysis"), recursive=TRUE)

# Initiate GRASS in 'PrepareStateClass' mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='LandCoverChangeRates')

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "HabitatSuitabilityAnalysis", flags="c")

# Import spatial data that isn't scenario-specific
execGRASS('g.copy', raster=c('MPB_mask@PrepareStateClass', 'MPB_mask'))
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
for(scenarioId in scenarioIDs){
  # ST-Sim outputs @ Scenario-level
        # Library
  library <- ssimLibrary(library_path)
  
        # Scenario
  scenario <- scenario(library, scenario = scenarioId)
  
        # Run Control information
  nIterations <- datasheet(scenario, "stsim_RunControl") %>% .$MaximumIteration
  minTime <- datasheet(scenario, "stsim_RunControl") %>% .$MinimumTimestep  
  maxTime <- datasheet(scenario, "stsim_RunControl") %>% .$MaximumTimestep
  timeSteps <- seq(from=minTime, to=maxTime, by=10)
  
        # Inputs - Get paths
  primaryStratum_name <- datasheet(scenario, "stsim_InitialConditionsSpatial") %>% .$StratumFileName
  secondaryStratum_name <- datasheet(scenario, "stsim_InitialConditionsSpatial") %>% .$SecondaryStratumFileName
  
        # Inputs - Import to GRASS mapset
  execGRASS('r.import', input=primaryStratum_name, output='primaryStratum')
  execGRASS('r.import', input=secondaryStratum_name, output='secondaryStratum')
  
  for(ts in timeSteps){
    
    for(it in 1:nIterations){
      # ST-Sim outputs @ iteration | timestep level
            # Get rasters
      output_stateClass <- datasheetRaster(scenario, "stsim_OutputSpatialState", iteration=it, timestep=ts)
      output_forestAge <- datasheetRaster(scenario, "stsim_OutputSpatialAge", iteration=it, timestep=ts)
      
            # Export as .tif
      dir.create(paste0(resultsDir, "Spatial/Temp"))
      writeRaster(output_stateClass, paste0(resultsDir, "Spatial/Temp/StateClass.tif"))
      writeRaster(output_forestAge, paste0(resultsDir, "Spatial/Temp/ForestAge.tif"))
      
            # Import to GRASS mapset
      execGRASS('r.import', input=paste0(resultsDir, "Spatial/Temp/StateClass.tif"), output='output_stateClass_inter')
      execGRASS('r.import', input=paste0(resultsDir, "Spatial/Temp/ForestAge.tif"), output='output_forestAge_inter')
      unlink(paste0(resultsDir, "Spatial/Temp"), recursive = T)
      
            # Convert rasters to integer
      execGRASS('r.mapcalc', expression='output_stateClass = int(output_stateClass_inter)')
      execGRASS('r.mapcalc', expression='output_forestAge = int(output_forestAge_inter)')
      execGRASS('g.remove', type='raster', name='output_stateClass_inter', 'f')
      execGRASS('g.remove', type='raster', name='output_forestAge_inter', 'f')
      
      # Habitat Suitability - Prepare inputs
            # Produce "Cut" raster (1 if area was cut; 0 otherwise)
      write.table(c('2=0', '3=0', '5=0', '6=0', '7=0', '9=0', '10=0', '11=0', '12=1', '13=0', '14=1', '*=NULL'), paste0(resultsDir, 'Tabular/Rules/StateClass_getCuts.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
      execGRASS('r.reclass', input='output_stateClass', output='output_cuts_inter', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getCuts.txt'), flags=c('overwrite'))
      execGRASS('r.mapcalc', expression='output_cuts = output_cuts_inter', region='current', flags='overwrite')
      execGRASS('g.remove', type='raster', name='output_cuts_inter', flags='f')
      
            # Produce "MPB" raster
      execGRASS('r.mapcalc', expression='mask_zero = if(MASK, 0)')
      execGRASS('r.patch', input=c('MPB_mask', 'mask_zero'), output='output_MPB')
      
            # Produce "Time-since-cut" raster
      execGRASS('r.mapcalc', expression="output_timeSinceCut_inter = if(output_cuts, output_forestAge, null())")
      execGRASS('r.mapcalc', expression='output_timeSinceCut = if(output_timeSinceCut_inter>40, 40, output_timeSinceCut_inter)', 'overwrite')
      execGRASS('g.remove', type='raster', name='output_timeSinceCut_inter', 'f')
      
            # Produce "Cut size" raster
                  # Clump raster
      execGRASS('r.clump', input='output_timeSinceCut', output='output_cutIds', 'overwrite')
      
                  # Compute cut sizes
      clumps <- execGRASS('r.stats', input='output_cutIds', 'c', intern=T) %>%
        as.data.frame(.)
      colnames(clumps) <- "V1"
      clumps %<>% separate(col=V1, into=c("ID", "nCells"), sep=' ') %>%
        .[1:(nrow(.)-2),] %>%
        mutate(nCells = as.integer(nCells))
      
                  # Create Cut size raster
      write.table(c(paste(clumps$ID, clumps$nCells, sep="="), '*=NULL'), paste0(resultsDir, 'Tabular/Rules/Cuts_getNCells.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
      execGRASS('r.reclass', input='output_cutIds', output='output_cutSize_nCells', rules=paste0(resultsDir, 'Tabular/Rules/Cuts_getNCells.txt'), flags=c('overwrite'))
      execGRASS('r.mapcalc', expression=paste0('output_cutSize = (float(output_cutSize_nCells)*',res,'*', res, ')/10000'), region='current', flags='overwrite')
      execGRASS('g.remove', type='raster', name='output_cutSize_nCells', flags='f')
      execGRASS('g.remove', type='raster', name='output_cutIds', flags='f')
      
      # Habitat Suitability - Union inputs
            # Polygonize
      execGRASS('r.to.vect', input='output_cuts', output='output_cuts_vect', type='area', 'overwrite')
      execGRASS('r.to.vect', input='output_MPB', output='output_MPB_vect', type='area', 'overwrite')
      execGRASS('r.to.vect', input='output_timeSinceCut', output='output_timeSinceCut_vect', type='area', 'overwrite')
      execGRASS('r.to.vect', input='output_cutSize', output='output_cutSize_vect', type='area', 'overwrite')
      
            # Union
      execGRASS('v.overlay', ainput='output_cuts_vect', binput='output_MPB_vect', operator='or', output='output_Cuts_MPB')
      execGRASS('v.overlay', ainput='output_Cuts_MPB', binput='output_timeSinceCut_vect', operator='or', output='output_Cuts_MPB_TimeSinceCut')
      execGRASS('v.overlay', ainput='output_Cuts_MPB_TimeSinceCut', binput='output_cutSize_vect', operator='or', output='output_all')
      
            # Organize columns
      execGRASS('v.db.renamecolumn', map='output_all', column=c('a_a_a_value', 'Cut'))
      execGRASS('v.db.renamecolumn', map='output_all', column=c('a_a_b_value', 'ModSevMPB'))
      execGRASS('v.db.renamecolumn', map='output_all', column=c('a_b_value', 'T_since_cut'))
      execGRASS('v.db.renamecolumn', map='output_all', column=c('b_value', 'Cut_size_ha'))
      execGRASS('v.db.dropcolumn', map='output_all', columns=c('a_cat', 'a_a_cat', 'a_a_a_cat', 'a_a_a_label', 'a_a_b_cat', 'a_a_b_label', 'a_b_cat', 'a_b_label', 'b_cat'))
      
            # Get attribute table
      att_outputs <- v.get.att('output_all', '&')
      
      # Habitat Suitability - Compute
      
      # Habitat Suitability - Calculate amount per stratum
            # Compute
      
            # Save tabular data
      
      # Habitat Suitability - Rasterize
            # Rasterize
      
            # Save raster
      
    }
    
    # Habitat Suitability - Average across all iterations
    
  }
}