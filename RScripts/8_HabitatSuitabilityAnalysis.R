#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser, adapted from script provided by Environment and Climate Change Canada

#### 8. Habitat Suitability Analysis

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a HabitatSuitabilityAnalysis GRASS mapset                                                                     #
# 2. Imports MPB data to the GRASS mapset                                                                                  #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. Loads ST-Sim library, project, and scenario                                                                           #
# 6. Produces a map of habitat suitability per target scenario, iteration, and timestep                                    #
# 5. Computes, in each case, the total amount of habitat suitability per primary stratum per secondary stratum             #
# 7. Produces a map of habitat suitability per timestep, by averaging across all iterations                                #
# 8. Saves outputs back to the ST-Sim library                                                                              #
############################################################################################################################

#### Mandatory Manual Step ####
# 1. In SQLiteStudio, go to table core_ScenarioResult
# 2. Identify the row(s) corresponding to the result scenario(s) of interest, and record the information contained within the row(s) for future reference
# 3. Delete the row(s)

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
scenarioIds <- c(30) # Can be one or multiple Scenario IDs
library_path <- paste0(resultsDir, "ssimLibrary/DawsonTSA.ssim.backup.2020-04-10-at-16-57-26/DawsonTSA.ssim")
  
# Statistical Models
      # Load models
m0 <- readRDS(paste0(tabularDataDir,"OSFL_uncut5.rds"))
m1 <- readRDS(paste0(tabularDataDir,"OSFL_cut4.rds"))

      # Scaling parameters
MeanTcut <- 10.74684
SDTcut <- 10.19146
MeanCutSz <- 51.59121
SDCutSz <- 33.01943

# Tabular data - Load
subzoneID <- read.xlsx(paste0(tabularDataDir, "BEC Subzone.xlsx"), sheet="Stratum") %>% # Load
  mutate(Name = ifelse(Name == 'SBSwc', 'SBSwk', ifelse(Name == 'ESSFmv2', 'ESSFmv', ifelse(Name == 'BWBSwk1', 'BWBSwk', Name)))) # Correct errors in .xlsx file
secondaryStratumID <- read.xlsx(paste0(tabularDataDir, 'SecondaryStratumID.xlsx'))

# ST-Sim outputs @ Project level
      # Library
library <- ssimLibrary(library_path)

      # Project
project <- project(library, project=1)

      # Add "OSFL Habitat" state attribute type
stsim_StateAttributeType <- datasheet(project, "stsim_StateAttributeType")
stsim_StateAttributeType <- addRow(stsim_StateAttributeType, 'OSFL Habitat')
saveDatasheet(project, stsim_StateAttributeType, "stsim_StateAttributeType")

      # Get "OSFL Habitat" key
stsim_StateAttributeType <- datasheet(project, "stsim_StateAttributeType", includeKey = T)
key <- stsim_StateAttributeType$StateAttributeTypeID[which(stsim_StateAttributeType$Name == 'OSFL Habitat')]

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
for(scenarioId in scenarioIds){
  # ST-Sim outputs @ Scenario-level
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
  
        # Inputs - Polygonize
              # Primary Stratum raster
  execGRASS('r.to.vect', input='primaryStratum', output='primaryStratum_vect', type='area', column='Stratum')
  
              # Secondary Stratum raster
  execGRASS('r.to.vect', input='secondaryStratum', output='secondaryStratum_vect', type='area', column='Stratum')
  
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
      execGRASS('r.import', input=paste0(resultsDir, "Spatial/Temp/StateClass.tif"), output='output_stateClass_inter', 'overwrite')
      execGRASS('r.import', input=paste0(resultsDir, "Spatial/Temp/ForestAge.tif"), output='output_forestAge_inter', 'overwrite')
      unlink(paste0(resultsDir, "Spatial/Temp"), recursive = T)
      
            # Convert rasters to integer
      execGRASS('r.mapcalc', expression='output_stateClass = int(output_stateClass_inter)', 'overwrite')
      execGRASS('r.mapcalc', expression='output_forestAge = int(output_forestAge_inter)', 'overwrite')
      execGRASS('g.remove', type='raster', name='output_stateClass_inter', 'f')
      execGRASS('g.remove', type='raster', name='output_forestAge_inter', 'f')
      
      # Habitat Suitability - Prepare inputs
            # Produce "Cut" raster (1 if area was cut; 0 otherwise)
      write.table(c('2=0', '3=0', '5=0', '6=0', '7=0', '9=0', '10=0', '11=0', '12=1', '13=0', '14=1', '*=NULL'), paste0(resultsDir, 'Tabular/Rules/StateClass_getCuts.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
      execGRASS('r.reclass', input='output_stateClass', output='output_cuts_inter', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getCuts.txt'), flags=c('overwrite'))
      execGRASS('r.mapcalc', expression='output_cuts = output_cuts_inter', region='current', flags='overwrite')
      execGRASS('g.remove', type='raster', name='output_cuts_inter', flags='f')
      
            # Produce "MPB" raster
      execGRASS('r.mapcalc', expression='mask_zero = if(MASK, 0)', 'overwrite')
      execGRASS('r.patch', input=c('MPB_mask', 'mask_zero'), output='output_MPB', 'overwrite')
      
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
      execGRASS('v.overlay', ainput='output_cuts_vect', binput='output_MPB_vect', operator='or', output='output_Cuts_MPB', 'overwrite')
      execGRASS('v.overlay', ainput='output_Cuts_MPB', binput='output_timeSinceCut_vect', operator='or', output='output_Cuts_MPB_TimeSinceCut', 'overwrite')
      execGRASS('v.overlay', ainput='output_Cuts_MPB_TimeSinceCut', binput='output_cutSize_vect', operator='or', output='habitatSuitability', 'overwrite')
      
            # Organize columns
      execGRASS('v.db.renamecolumn', map='habitatSuitability', column=c('a_a_a_value', 'Cut'))
      execGRASS('v.db.renamecolumn', map='habitatSuitability', column=c('a_a_b_value', 'ModSevMPB'))
      execGRASS('v.db.renamecolumn', map='habitatSuitability', column=c('a_b_value', 'T_since_cut'))
      execGRASS('v.db.renamecolumn', map='habitatSuitability', column=c('b_value', 'Cut_size_ha'))
      execGRASS('v.db.dropcolumn', map='habitatSuitability', columns=c('a_cat', 'a_a_cat', 'a_a_a_cat', 'a_a_a_label', 'a_a_b_cat', 'a_a_b_label', 'a_b_cat', 'a_b_label', 'b_cat'))
      
            # Format attribute table
      att_habitatSuitability <- v.get.att('habitatSuitability', '&') %>%
        mutate(PCYear = 2025) %>%
        mutate(Cut = as.factor(Cut),
               PCYear = as.factor(PCYear),
               ModSevMPB = as.factor(ModSevMPB))
      
      # Habitat Suitability - Compute
            # Scale variables
      att_habitatSuitability$T_cut_sc <- ifelse(att_habitatSuitability$Cut=="1", (att_habitatSuitability$T_since_cut - MeanTcut)/SDTcut, NA)
      att_habitatSuitability$Cut_size_sc <- ifelse(att_habitatSuitability$Cut=="1", (att_habitatSuitability$Cut_size_ha - MeanCutSz)/SDCutSz,NA)
      
            # Add quadratic effect for T_since_cut
      att_habitatSuitability$T_cut_sc_sq <- (att_habitatSuitability$T_cut_sc)^2
      
            # Predict OSFL presence using m1 for cut sites and m0 for uncut sites
      att_habitatSuitability$preds <- ifelse(att_habitatSuitability$Cut==1,
                                             predict(m1, newdata=att_habitatSuitability, type="response", allow.new.levels=T),
                                             predict(m0, newdata=att_habitatSuitability, type="response", allow.new.levels=T))
      
            # Add to attribute table
      execGRASS('v.db.addcolumn', map='habitatSuitability', columns='HabitatSuitability double precision')
      for(i in 1:ceiling(nrow(att_habitatSuitability)/50)){
        start <- (i-1)*50+1
        end <- ifelse(i==ceiling(nrow(att_habitatSuitability)/50), nrow(att_habitatSuitability), i*50)
        rule <- paste('CASE', paste(paste('WHEN cat =', att_habitatSuitability$cat[start:end], 'THEN', att_habitatSuitability$preds[start:end]), collapse=" "), 'ELSE HabitatSuitability END')
        execGRASS('v.db.update', map='habitatSuitability', column='HabitatSuitability', query_column=rule)
        print(paste(i, "of", ceiling(nrow(att_habitatSuitability)/50), ":", Sys.time()))
      }
      
      # Habitat Suitability - Rasterize
      execGRASS('v.to.rast', input='habitatSuitability', output=paste0('habitatSuitability_', it), use='attr', attribute_column='HabitatSuitability', 'overwrite')
      
      # Habitat Suitability - Calculate amount per stratum
            # Union Stratum and Habitat Suitability
      execGRASS('v.overlay', ainput='habitatSuitability', binput="primaryStratum_vect", operator="or", output='PrimaryStratum_Habitat', 'overwrite')
      execGRASS('v.overlay', ainput='PrimaryStratum_Habitat', binput="secondaryStratum_vect", operator="or", output='Stratum_Habitat', 'overwrite')
      
            # Habitat suitability amount | Primary Stratum | Secondary Stratum
      att_stratumHabitat <- v.get.att("Stratum_Habitat", "&") %>%
        rename(HabitatSuitability = a_a_HabitatSuitability, StratumID = a_b_Stratum, SecondaryStratumID = b_Stratum) %>%
        group_by(StratumID, SecondaryStratumID) %>%
        summarize(Amount = sum(HabitatSuitability)) %>%
        ungroup() %>%
        mutate(Iteration = it, Timestep = ts) %>%
        dplyr::select(Iteration, Timestep, StratumID, SecondaryStratumID, Amount)
      att_stratumHabitat$StratumID <- sapply(att_stratumHabitat$StratumID, function(x) subzoneID$Name[which(subzoneID$ID == x)])
      att_stratumHabitat$SecondaryStratumID <- sapply(att_stratumHabitat$SecondaryStratumID, function(x) secondaryStratumID$Name[which(secondaryStratumID$Secondary.Stratum.ID == x)])
      
            # Save
      if(!exists("finalTable")){
        finalTable <- att_stratumHabitat
      }else{
        finalTable <- bind_rows(finalTable, att_stratumHabitat)
      }
    }
    # Habitat Suitability - Average across all iterations
          # Compute
    maps <- paste0('HabitatSuitability_', 1:nIterations)
    execGRASS('r.mapcalc', expression = paste0('HabitatSuitability_avg = (', paste0(maps, collapse="+"), ')/', nIterations), 'overwrite')
    
          # Convert to raster
    execGRASS('r.out.gdal', input='HabitatSuitability_avg', output=paste0(resultsDir, "Spatial/DataLayers/HabitatSuitability_Scenario", scenarioId, "_Timestep", ts, ".tif"), 'overwrite')
    habitatSuitability <- raster(paste0(resultsDir, "Spatial/DataLayers/HabitatSuitability_Scenario", scenarioId, "_Timestep", ts, ".tif"))
    
          # Save to ST-Sim library
    filename <- paste0("sa_", key, ".it1.ts", ts, ".tif")
    data <- data.frame(Iteration = 1,
                       Timestep = ts,
                       StateAttributeTypeID = "OSFL Habitat",
                       Filename = filename)
    fileData <- setNames(habitatSuitability, filename)
    saveDatasheet(scenario, data=data, name="stsim_OutputSpatialStateAttribute", fileData=fileData, append=T)
  }
  # Save tabular data to ST-Sim library
  finalTable %<>% mutate(StateAttributeTypeID = "OSFL Habitat",
                         AgeClass = "20")
  saveDatasheet(scenario, data=finalTable, name='stsim_OutputStateAttribute', append=T)
  write.csv(finalTable, paste0(resultsDir, "Tabular/OutputStateAttribute.csv"), row.names = F)
}

#### Mandatory Manual Step ####
# 1. In SQLiteStudio, go to table core_ScenarioResult
# 2. Re-populate the row(s) that you deleted at the top of the script
