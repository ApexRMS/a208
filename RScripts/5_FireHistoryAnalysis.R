#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 5. Fire History Analysis

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a FireHistoryAnalysis GRASS mapset                                                                            #
# 2. Imports Primary Stratum data and Fire Perimeters data to GRASS mapset                                                 #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. Computes area burned per year and Primary Stratum                                                                     #
# 5. Determines the fire size class distribution                                                                           #
############################################################################################################################

#### Workspace ####
# Packages
library(rgrass7)
library(tidyverse)
library(magrittr)
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

#### Fire History Analysis ####
# Polygonize Primary Stratum raster
execGRASS('r.to.vect', input='primaryStratum', output='primaryStratum_vect', type='area', column='Stratum')

# Clip Fire Perimeters to study area
execGRASS("v.clip", input='rawData_FirePerimeters', clip='primaryStratum_vect', output='firePerimeters_area')

# Union Primary Stratum and Fire Perimeters
execGRASS('v.overlay', ainput='primaryStratum_vect', binput='firePerimeters_area', operator="or", output='StratumUnionFire')

# Area burned per stratum | year
      # Compute area of each polygon
execGRASS('v.db.addcolumn', map='StratumUnionFire', columns='Area double precision')
execGRASS('v.to.db', map='StratumUnionFire', columns='Area', option='area', units='meters')

      # Get attribute table
att_StratumUnionFire <- v.get.att("StratumUnionFire", "&")

      # Compute area burned per stratum | year
areaBurned_Stratum_Year <- att_StratumUnionFire %>%
  drop_na(b_cat) %>%
  group_by(a_Stratum, b_FIRE_YEAR) %>%
  summarize(AreaBurned_m2 = sum(Area)) %>%
  rename(Stratum = a_Stratum, Year = b_FIRE_YEAR) %>%
  arrange(Stratum, Year) %>%
  mutate(AreaBurned_km2 = AreaBurned_m2/1000000) %>%
  select(-AreaBurned_m2)

# Total area per stratum
      # Compute area of each polygon
execGRASS('v.db.addcolumn', map='primaryStratum_vect', columns='Area double precision')
execGRASS('v.to.db', map='primaryStratum_vect', columns='Area', option='area', units='meters')

      # Get attribute table
att_PrimaryStratum <- v.get.att("primaryStratum_vect", "&")

      # Compute total area per stratum
area_Stratum <- att_PrimaryStratum %>%
  group_by(Stratum) %>%
  summarize(TotalArea_m2 = sum(Area)) %>%
  arrange(Stratum) %>%
  mutate(TotalArea_km2 = TotalArea_m2/1000000) %>%
  select(-TotalArea_m2)

      # Compute percentage burned per stratum
areaBurned_Stratum_Year %<>% left_join(., area_Stratum, by='Stratum') %>%
  mutate(PercentBurned = AreaBurned_km2/TotalArea_km2)

      # Export
write.xlsx(areaBurned_Stratum_Year, paste0(resultsDir, 'Tabular/AreaBurned_ByStratum_ByYear.xlsx'))

# Fire count by fire size class
      # Get attribute table
att_FirePerimeters <- v.get.att("firePerimeters_area", "&")

      # Get fire sizes in km2
fireSizes <- att_FirePerimeters %>%
  rename(Area_ha = FIRE_SIZE_) %>%
  select(Area_ha) %>%
  arrange(Area_ha)

      # Define size classes
mins <- c(0, 1, 10, 100, 1000, 10000)
maxs <- c(1, 10, 100, 1000, 10000, ceiling(max(fireSizes)))
  
      # Compute fire count by fire size class
fireCount_SizeClass <- data.frame(SizeClass_Min_ha = mins,
                                  SizeClass_Max_ha = maxs)
fireCount_SizeClass$FireCount <- sapply(1:nrow(fireCount_SizeClass),
                                        function(x) length(fireSizes[which((fireSizes$Area_ha >= fireCount_SizeClass$SizeClass_Min_ha[x]) & (fireSizes$Area_ha < fireCount_SizeClass$SizeClass_Max_ha[x])),]))

      # Export
write.csv(fireSizes, paste0(resultsDir, "Tabular/FireSizes.csv"), row.names=F)
write.csv(fireCount_SizeClass, paste0(resultsDir, "Tabular/FireCount_bySizeClass.csv"), row.names=F)
