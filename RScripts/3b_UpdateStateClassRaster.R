#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 3b. Update State Class Raster

##################################################################################################################################################################
# This code updates the State Class raster created in R code 3, as follows:                                                                                      #
# 1. Any cells having experienced a MPB outbreak of moderate or greater severity are reclassified as Pine (even if they had <10% pine in the VRI)                #
# 2. Any cells having experienced a fire after 1980 are reclassified as disturbed (unless a MPB outbreak occurred after the fire, in which case they remain MPB) #
##################################################################################################################################################################

# Workspace
library(tidyverse)
library(magrittr)
library(raster)
library(fasterize)
library(sf)
library(rgdal)

# Directories
dataDir <- "E:/a208/Data/"

# Data
stateClass_old <- raster(paste0(dataDir, "StateClassAndAge/StateClass.tif"))
fire <- st_read(paste0(dataDir, "FirePerimeters/FirePerimeters_WGS84_UTM10N.shp"))
mpb <- st_read(paste0(dataDir, "MPB/MPB.shp"))

# Rasterize MBP
      # Only retain moderate, severe, and very severe outbreaks
mpb %<>% filter(PST_SEV_CD %in% c("M", "S", "V"))

      # Project to study CRS
mpb %<>% st_transform(st_crs(stateClass_old))

      # Rasterize, using age as the raster value (and prioritizing the most recent outbreak)
r_mpb <- fasterize(mpb, stateClass_old, field="CPTR_YR", fun='max')

      # Save
writeRaster(r_mpb, paste0(dataDir, "MPB.tif"), datatype = "FLT4S")

# Rasterize Fire
      # Only retain fires that occured after 1980
fire %<>% filter(FIRE_YEAR > 1980)
  
      # Project to study CRS
fire %<>% st_transform(st_crs(stateClass_old))

      # Rasterize, using age as the raster value
r_fire <- fasterize(fire, stateClass_old, field="FIRE_YEAR", fun='max')

      # Save
writeRaster(r_fire, paste0(dataDir, "Fire.tif"), datatype = "FLT4S")

# New State Class raster
      # Get disturbance type (1 = MPB, 2 = Fire)
disturbanceType <- overlay(r_mpb, r_fire, fun=function(x,y){
  result <- ifelse(!is.na(x) & is.na(y),
                   1, 
                   ifelse(is.na(x) & !is.na(y),
                          2,
                          ifelse(x>=y,
                                 1,
                                 2)))
  return(result)
})

      # Reclassify any cell having experienced a MPB outbreak as Pine
stateClass_inter <- overlay(stateClass_old, r_mpb, fun=function(x,y){
  result <- ifelse((x %in% c(1:12))| is.na(y) | is.na(x), # If not forested or already pine, or if no MPB outbreak of severity moderate or greater
                   x,
                   ifelse(x == 13,
                          10,
                          12))
  return(result)
})

      # Compute new state classes
stateClass_new <- overlay(stateClass_inter, disturbanceType, fun=function(x,y){
  result <- ifelse((x %in% c(1:9, 12, 14)) | is.na(y) | is.na(x), # If not forested or already disturbed, or if there is no disturbanceType, keep old state class
                   x,
                   ifelse(x %in% c(10, 11), 
                          ifelse(y==1, # Pine forests
                                 11,
                                 12),
                          ifelse(y==2, # Non-pine forests
                                 14,
                                 x)))
  
  return(result)
})

      # Save
writeRaster(stateClass_new, paste0(dataDir, "NewStateClass.tif"), overwrite=T)

# Compare old and new raster
      # Raster comparison
comparison <- overlay(stateClass_old, stateClass_new, fun=function(x,y){
  result <- ifelse(x == y,
                   0,
                   x*100+y)
  return(result)
})

      # Tabular comparison
t <- freq(comparison)
t %<>% as.data.frame() %>%
  filter(!is.na(value)) %>%
  mutate(percentage = 100*count/sum(count),
         oldValue = value %/% 100,
         newValue = value %% 100) %>%
  dplyr::select(-value) %>%
  arrange(oldValue, newValue)

      # Save
writeRaster(comparison, paste0(dataDir, "Comparison.tif"))
write.csv(t, paste0(dataDir, "Comparison.csv"))
