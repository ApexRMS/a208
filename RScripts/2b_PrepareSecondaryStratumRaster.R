#------------------------------------------------------------
# protectedAreas.R
# Reclassify ownership raster to increase protected areas to
# 30% of land base.
# Increases protection by adding unprotected MPB forest older
# than 75 years old.
# Leonardo Frid
# ApexRMS
# 2021-03-31
#------------------------------------------------------------

library(rsyncrosim)
library(rgdal)
library(raster)

setwd("d:/A246")
getwd()
myLib = ssimLibrary("DawsonTSA.ssim")
myScenario = scenario(myLib, scenario="Initial Conditions")
datasheet(myScenario)

myDatasheet = datasheet(myScenario, name = "InitialConditionsSpatial")

secondaryStratumRaster = datasheetRaster(myScenario, "InitialConditionsSpatial", 
                                      column = "SecondaryStratumFileName")
plot(secondaryStratumRaster)

sClassRaster = datasheetRaster(myScenario, "InitialConditionsSpatial", 
                               column = "StateClassFileName")
plot(sClassRaster)

ageRaster = datasheetRaster(myScenario, "InitialConditionsSpatial", 
                            column = "AgeFileName")
plot(ageRaster)

# Forest older than 75 years
rcl = matrix(c(-0.5, 75.5,0,75.5,1000,1), nrow = 2, ncol = 3, byrow = T)
oldGrowthRaster = reclassify(ageRaster,rcl)
plot(oldGrowthRaster)

# Forest but not MPB
rcl = matrix(c(1.5,9.5,0,9.5,10.5, 1, 10.5, 11.5, 0, 11.5, 14.5, 1),nrow = 4, ncol = 3, byrow = T)
forestRaster = reclassify(sClassRaster, rcl)
plot(forestRaster)

# Available public lands
rcl = matrix(c(0.5,2.5, 0, 2.5,3.5, 1), nrow = 2, ncol = 3, byrow = T)
areasToProtectRaster = reclassify(secondaryStratumRaster, rcl)
plot(areasToProtectRaster)

# Old growth forest to protect
oldGrowthProtectRaster = oldGrowthRaster*forestRaster*areasToProtectRaster
plot(oldGrowthProtectRaster)
freq(oldGrowthProtectRaster)

# MPB cells
rcl = matrix(c(0,10.5,0,10.5,11.5,1,11.5, 14.5,0), nrow = 3, ncol = 3, byrow = T)
mpbRaster = reclassify(sClassRaster, rcl)
plot(mpbRaster)

mpbToProtectRaster = areasToProtectRaster*mpbRaster
plot(mpbToProtectRaster)

oldMpbToProtectRaster = mpbToProtectRaster * oldGrowthRaster
plot(oldMpbToProtectRaster)

# New protected areas
newProtectedRaster = oldMpbToProtectRaster #+ oldGrowthProtectRaster
plot(newProtectedRaster)
freq(newProtectedRaster)

# New secondary stratum raster
# cells with ID 4 are the new protected areas.
newSSRaster = newProtectedRaster + secondaryStratumRaster
plot(newSSRaster)
freq(newSSRaster)

writeRaster(newSSRaster, "newSS.tif")

rcl = matrix(c(3.5,4.5,1), nrow = 1, ncol = 3, byrow = T)
finalSSRaster = reclassify(newSSRaster, rcl)
plot(finalSSRaster)


writeRaster(finalSSRaster, "SecondaryStratum2.tif")
