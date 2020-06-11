#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser, adapted from script provided by Environment and Climate Change Canada

#### 8. Habitat Suitability Analysis

############################################################################################################################
# This code:                                                                                                               #
# 1. Loads ST-Sim library, project, and scenario                                                                           #
# 2. Produces a map of habitat suitability per target scenario, iteration, and timestep                                    #
# 3. Computes, in each case, the total amount of habitat suitability per primary stratum per secondary stratum             #
# 4. Produces a map of habitat suitability per timestep, by averaging across all iterations                                #
# 5. Saves outputs back to the ST-Sim library                                                                              #
############################################################################################################################

#### Workspace ####
# Get start time
start_time <- Sys.time()

# Packages
library(RSQLite)
library(tidyverse)
library(magrittr)
library(stringr)
library(openxlsx)
library(rsyncrosim)
library(raster)
library(blme)
library(doParallel)

# Settings
Sys.setenv(TZ='GMT')
options(stringsAsFactors=FALSE, SHAPE_RESTORE_SHX=T, useFancyQuotes = F, digits=10)

# Directories
spatialDataDir <- "E:/a208/Data/Spatial/"
tabularDataDir <- "E:/a208/Data/Tabular/"
resultsDir <- "E:/a208/Results/"

# Input Parameters
scenarioIds <- c(30) # Can be one or multiple Scenario IDs

# File Paths
      # ST-Sim library
library_path <- paste0(resultsDir, "ssimLibrary/DawsonTSA.ssim.backup.2020-04-10-at-16-57-26_CD_2020.06.11/DawsonTSA.ssim") # Path to ST-Sim library

      # Statistical models
m0_path <- paste0(tabularDataDir,"OSFL_uncut5.rds") # Path to ECCC m0 model for OSFL
m1_path <- paste0(tabularDataDir,"OSFL_cut4.rds") # Path to ECCC m1 model for OSFL

#### Load Data ####
# Statistical Models
      # Load models
m0 <- readRDS(m0_path)
m1 <- readRDS(m1_path)

      # Scaling parameters
MeanTcut <- 10.74684
SDTcut <- 10.19146
MeanCutSz <- 51.59121
SDCutSz <- 33.01943

      # Coefficients
m0_ModSevMPB1_mean <- summary(m0)$coefficients[2,1]
m0_ModSevMPB1_se <- summary(m0)$coefficients[2,2]

m1_Cut_size_sc_mean <- summary(m1)$coefficients[2,1]
m1_Cut_size_sc_se <- summary(m1)$coefficients[2,2]

m1_T_cut_sc_sq_mean <- summary(m1)$coefficients[4,1]
m1_T_cut_sc_sq_se <- summary(m1)$coefficients[4,2]

# ST-Sim outputs @ Project level
      # SQLite connection
db <- dbConnect(SQLite(), dbname=library_path)

      # Library
library <- ssimLibrary(library_path)

      # Project
project <- rsyncrosim::project(library, project=1)

      # Add "OSFL Habitat" state attribute type
stsim_StateAttributeType <- datasheet(project, "stsim_StateAttributeType")
stsim_StateAttributeType <- addRow(stsim_StateAttributeType, 'OSFL Habitat')
saveDatasheet(project, stsim_StateAttributeType, "stsim_StateAttributeType")

      # Get "OSFL Habitat" key
stsim_StateAttributeType <- datasheet(project, "stsim_StateAttributeType", includeKey = T)
key <- stsim_StateAttributeType$StateAttributeTypeID[which(stsim_StateAttributeType$Name == 'OSFL Habitat')]

#### Create Cluster ####
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

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
  
        # Inputs - Import
  primaryStratum <- raster(primaryStratum_name)
  secondaryStratum <- raster(secondaryStratum_name)
  
        # Inputs - Get keys
  keys_primaryStratum <- datasheet(scenario, name = "stsim_Stratum", includeKey = T)
  keys_secondaryStratum <- datasheet(scenario, name = "stsim_SecondaryStratum", includeKey = T)
  
  # Create empty raster
  mask <- datasheetRaster(scenario, "stsim_OutputSpatialState", iteration=1, timestep=timeSteps[1])
  mask[] <- as.numeric(mask[])
  mask[!is.na(mask)] <- 0
  
  # Define statistical models to be used for each iteration
  m0_list <- list()
  m1_list <- list()
  
  for(it in 1:nIterations){
    # m0
    m0_it <- m0
    m0_it@beta[2] <- rnorm(1, mean = m0_ModSevMPB1_mean, sd = m0_ModSevMPB1_se)
    m0_list <- c(m0_list, m0_it)
    
    # m1
    m1_it <- m1
    m1_it@beta[2] <- rnorm(1, mean = m1_Cut_size_sc_mean, sd = m1_Cut_size_sc_se)
    m1_it@beta[4] <- rnorm(1, mean = m1_T_cut_sc_sq_mean, sd = m1_T_cut_sc_sq_se)
    m1_list <- c(m1_list, m1_it)
  }
  rm(it, m0_it, m1_it)
  
  for(ts in timeSteps){
    
    # Parallelize over iterations
    habitatSuitability_output <- foreach(it=1:nIterations, .packages=c('rsyncrosim', 'raster', 'tidyverse')) %dopar% {
      # Grab statistical models corresponding to the iteration of interest
      m0_it <- m0_list[[it]]
      m1_it <- m1_list[[it]]
      
      # ST-Sim outputs @ iteration | timestep level
            # Get rasters
      stateClass <- datasheetRaster(scenario, "stsim_OutputSpatialState", iteration=it, timestep=ts)
      forestAge <- datasheetRaster(scenario, "stsim_OutputSpatialAge", iteration=it, timestep=ts)
      
            # Convert rasters to integer
      stateClass[] <- as.integer(stateClass[])
      forestAge[] <- as.integer(forestAge[])
      
      # Habitat Suitability - Prepare inputs
            # Produce "Cuts" raster (1 if area was cut; 0 otherwise)
      reclassRules <- matrix(c(2, 0, 3, 0, 5, 0, 6, 0, 7, 0, 9, 0, 10, 0, 11, 0, 12, 1, 13, 0, 14, 1), ncol=2, byrow=T)
      cuts <- reclassify(x=stateClass, rcl=reclassRules)
      cuts[] <- as.integer(cuts[])
      
            # Prodice "MPB" raster (1 if area experienced moderate, severe, or very severe MPB; 0 otherwise)
      reclassRules <- matrix(c(2, 0, 3, 0, 5, 0, 6, 0, 7, 0, 9, 0, 10, 0, 11, 1, 12, 0, 13, 0, 14, 0), ncol=2, byrow=T)
      MPB <- reclassify(x=stateClass, rcl=reclassRules)
      MPB[] <- as.integer(MPB[])
      
            # Produce "Time-since-cut" raster
                  # Create empty raster
      timeSinceCut <- mask
      timeSinceCut[] <- NA
      
                  # Compute new value for each cell
      df <- data.frame(Cut=cuts[], ForestAge=forestAge[]) %>%
        mutate(TimeSinceCut = ifelse(is.na(Cut),
                                     NA,
                                     ifelse(Cut == 0,
                                            NA,
                                            ifelse(ForestAge > 40,
                                                   40,
                                                   ForestAge))))
      
                  # Populate empty raster
      timeSinceCut[] <- df$TimeSinceCut
      
            # Produce "Cut size" raster
                  # Clump timeSinceCut raster
                        # Create empty raster
      cutIds <- cuts
      cutIds[] <- NA
      
                        # Populate empty raster with cutIds
      uniqueAges <- sort(unique(timeSinceCut[]))
      for(age in uniqueAges){
        # Create raster for timeSinceCut = Age
        ageRaster <- setValues(raster(timeSinceCut), NA)
        ageRaster[timeSinceCut == age] <- 1
        
        # Clump resulting raster
        cutIds_age <- clump(ageRaster, directions=8, gaps=F)
        
        # Make sure cutIds are unique
        if(!age == uniqueAges[1]){
          cutIds_age <- cutIds_age + max(cutIds[], na.rm=T)
        }
        
        # Add cutIds to emptyRaster
        cutIds <- merge(cutIds_age, cutIds, overlap=T)
      }
      
                  # Compute cut sizes
      df_cutSizes <- as.data.frame(freq(cutIds)) %>%
        filter(!is.na(value)) %>%
        mutate(area_ha = count*res(cuts)[1]*res(cuts)[1]/10000) %>%
        dplyr::select(-count)
      
                  # Create cut size raster
      cutSizes <- reclassify(cutIds, df_cutSizes)
      
      # Habitat Suitability - Compute
            # Get input parameters
      df_habitatSuitability <- data.frame(StateClass = stateClass[], Cut = cuts[], ModSevMPB = MPB[], T_since_cut = timeSinceCut[], Cut_size_ha = cutSizes[], PCYear = 2025) %>%
        mutate(Cut = as.factor(Cut),
               PCYear = as.factor(PCYear),
               ModSevMPB = as.factor(ModSevMPB))
      
            # Scale variables
      df_habitatSuitability$T_cut_sc <- ifelse(df_habitatSuitability$Cut==1, (df_habitatSuitability$T_since_cut - MeanTcut)/SDTcut, NA)
      df_habitatSuitability$Cut_size_sc <- ifelse(df_habitatSuitability$Cut==1, (df_habitatSuitability$Cut_size_ha - MeanCutSz)/SDCutSz,NA)
      
            # Add quadratic effect for T_since_cut
      df_habitatSuitability$T_cut_sc_sq <- (df_habitatSuitability$T_cut_sc)^2
      
            # Predict OSFL presence using m1 for cut sites and m0 for uncut sites
      df_habitatSuitability$preds <- ifelse(df_habitatSuitability$StateClass < 10, # If not forest, set habitat suitability to 0
                                            0,
                                            ifelse(df_habitatSuitability$Cut==1,
                                                   predict(m1_it, newdata=df_habitatSuitability, type="response", allow.new.levels=T),
                                                   predict(m0_it, newdata=df_habitatSuitability, type="response", allow.new.levels=T)))
      
            # Create Habitat Suitability raster
      habitatSuitability <- mask
      habitatSuitability[] <- NA
      habitatSuitability[] <- df_habitatSuitability$preds
      
            # Save
      return(habitatSuitability)
    }
    
    # Make result a stack
    habitatSuitability_stack <- stack(habitatSuitability_output)
    names(habitatSuitability_stack) <- 1:nIterations
    rm(habitatSuitability_output)
    
    # Prepare and save outputs
          # Format result as tabular data
    names <- names(habitatSuitability_stack)
    df_habitatSuitability <- data.frame(StratumID = primaryStratum[], SecondaryStratumID = secondaryStratum[])
    for(name in names){
      df_habitatSuitability[, name] <- habitatSuitability_stack[[name]][]
    }  
    
          # SpatialStateAttribute output
                # Filename
    filename <- paste0("sa_", key, ".it1.ts", ts, ".tif")
    
                # Spatial output
                      # Compute average
    df_habitatSuitability_avg <- df_habitatSuitability %>%
      dplyr::select(-c(StratumID, SecondaryStratumID)) %>%
      mutate(Average = rowMeans(.))
    
                      # Rasterize
    habitatSuitability <- mask
    habitatSuitability[] <- NA
    habitatSuitability[] <- df_habitatSuitability_avg$Average
    
                      # Save
    path <- library_path %>%
      str_locate_all(., "/") %>%
      as.data.frame() %>%
      .[nrow(.),] %>%
      .[1,1] %>%
      substr(library_path, start=1, stop=.) %>%
      paste0(., "DawsonTSA.ssim.output/Scenario-", scenarioId, "/stsim_OutputSpatialStateAttribute/", filename)
    writeRaster(habitatSuitability, path, overwrite=T)
    
                # Tabular output
                      # Prepare
    data <- data.frame(ScenarioID = scenarioId,
                       Iteration = 1,
                       Timestep = ts,
                       StateAttributeTypeID = key,
                       Filename = filename)
    
                      # Save
    dbWriteTable(conn=db, name="stsim_OutputSpatialStateAttribute", value=data, append=T, row.names=F)
    
          # StateAttribute output
                # Prepare
    df_stratumHabitat <- df_habitatSuitability %>%
      drop_na() %>%
      gather(., key="Iteration", value="HabitatSuitability", -c('StratumID', 'SecondaryStratumID')) %>%
      mutate(Iteration = substr(Iteration, start=2, stop=nchar(Iteration))) %>%
      group_by(StratumID, SecondaryStratumID, Iteration) %>%
      summarize(Amount = sum(HabitatSuitability)) %>%
      ungroup() %>%
      mutate(ScenarioID = scenarioId, Timestep = ts, StateAttributeTypeID = key) %>%
      dplyr::select(ScenarioID, Iteration, Timestep, StratumID, SecondaryStratumID, StateAttributeTypeID, Amount)
    df_stratumHabitat$StratumID <- sapply(df_stratumHabitat$StratumID, function(x) keys_primaryStratum$StratumID[which(keys_primaryStratum$ID == x)])
    df_stratumHabitat$SecondaryStratumID <- sapply(df_stratumHabitat$SecondaryStratumID, function(x) keys_secondaryStratum$SecondaryStratumID[which(keys_secondaryStratum$ID == x)])
    
                # Save
    dbWriteTable(conn=db, name="stsim_OutputStateAttribute", value=df_stratumHabitat, append=T, row.names=F)
    write.csv(df_stratumHabitat, paste0(resultsDir, "Tabular/OutputStateAttribute.csv"), row.names = F)
    
    print(paste0("Scenario: ", scenarioId, "; Timestep: ", ts))
  }
}

# Stop cluster
stopCluster(cl)

# Get total run time
print(start_time - Sys.time())
