#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 7. Land Cover Change Rates

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a LandCoverChangeRates GRASS mapset                                                                           #
# 2. Imports Secondary Stratum and LULC data for 1990, 2000, and 2010 to GRASS mapset                                      #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. Characterizes 1990-2000 and 2000-2010 land cover change                                                               #
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

# Tabular data - Load
secondaryStratumID <- read.xlsx(paste0(tabularDataDir, 'SecondaryStratumID.xlsx'))
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
# Initiate GRASS in 'PrepareStateClass' mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='LoggingHistoryAnalysis')

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "LandCoverChangeRates", flags="c")

# Import data
execGRASS('g.copy', raster=c('LC1990_agg_mask@PreparePrimaryStratum', 'LC1990_agg_mask'))
execGRASS('g.copy', raster=c('LC2000_agg_mask@PreparePrimaryStratum' ,'LC2000_agg_mask'))
execGRASS('g.copy', raster=c('LC2010_agg_mask@PreparePrimaryStratum' ,'LC2010_agg_mask'))
execGRASS('g.copy', raster=c('secondaryStratum@PrepareSecondaryStratum', 'secondaryStratum'))
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

#### Land Cover Change Rates ####
# Simplify land cover classes
      # 1990
execGRASS('r.reclass', input='LC1990_agg_mask', output='LC1990_broad_mask_inter', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getBroadLandCover.txt'), flags=c('overwrite'))
execGRASS('r.mapcalc', expression='LC1990_broad_mask = LC1990_broad_mask_inter', region='current', flags='overwrite')
execGRASS('g.remove', type='raster', name='LC1990_broad_mask_inter', flags='f')

      # 2000
execGRASS('r.reclass', input='LC2000_agg_mask', output='LC2000_broad_mask_inter', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getBroadLandCover.txt'), flags=c('overwrite'))
execGRASS('r.mapcalc', expression='LC2000_broad_mask = LC2000_broad_mask_inter', region='current', flags='overwrite')
execGRASS('g.remove', type='raster', name='LC2000_broad_mask_inter', flags='f')

      # 2010
execGRASS('r.reclass', input='LC2010_agg_mask', output='LC2010_broad_mask_inter', rules=paste0(resultsDir, 'Tabular/Rules/StateClass_getBroadLandCover.txt'), flags=c('overwrite'))
execGRASS('r.mapcalc', expression='LC2010_broad_mask = LC2010_broad_mask_inter', region='current', flags='overwrite')
execGRASS('g.remove', type='raster', name='LC2010_broad_mask_inter', flags='f')

# Create rasters tracking both Ownership and LC
execGRASS('r.mapcalc', expression = 'Ownership_LC1990 = SecondaryStratum*10 + LC1990_broad_mask')
execGRASS('r.mapcalc', expression = 'Ownership_LC2000 = SecondaryStratum*10 + LC2000_broad_mask')
execGRASS('r.mapcalc', expression = 'Ownership_LC2010 = SecondaryStratum*10 + LC2010_broad_mask')

# Compute transition matrices
execGRASS('r.kappa', reference='Ownership_LC1990', classification='Ownership_LC2000', output=paste0(resultsDir, "Tabular/TransitionMatrix_1990to2000.csv"), flags=c('overwrite', "h", 'w'))
execGRASS('r.kappa', reference='Ownership_LC2000', classification='Ownership_LC2010', output=paste0(resultsDir, "Tabular/TransitionMatrix_2000to2010.csv"), flags=c('overwrite', "h", 'w'))

# Format transition matrices
      # Load as data frames
tm_1990_2000 <- read.csv(paste0(resultsDir, "Tabular/TransitionMatrix_1990to2000.csv"), sep="\t", header=F)
tm_2000_2010 <- read.csv(paste0(resultsDir, "Tabular/TransitionMatrix_2000to2010.csv"), sep="\t", header=F)

      # Format to dataframe with col1 = time 1 and colnames = time 2
            # Get each panel
tm_1990_2000_p1 <- tm_1990_2000[4:25, 1:10]
tm_1990_2000_p2 <- tm_1990_2000[29:50, 1:10]
tm_1990_2000_p3 <- tm_1990_2000[54:75, 1:4]

tm_2000_2010_p1 <- tm_2000_2010[4:25, 1:10]
tm_2000_2010_p2 <- tm_2000_2010[29:50, 1:10]
tm_2000_2010_p3 <- tm_2000_2010[54:75, 1:4]

            # Bind panels
tm_1990_2000 <- bind_cols(tm_1990_2000_p1, tm_1990_2000_p2[, 2:10], tm_1990_2000_p3[, 2:4])
colnames(tm_1990_2000) <- c('1990 (C) to 2000 (R)', tm_1990_2000[1, 2:ncol(tm_1990_2000)])
tm_1990_2000 <- tm_1990_2000[2:nrow(tm_1990_2000),]
tm_1990_2000[,1] <- colnames(tm_1990_2000)[2:ncol(tm_1990_2000)]
rm(tm_1990_2000_p1, tm_1990_2000_p2, tm_1990_2000_p3)

tm_2000_2010 <- bind_cols(tm_2000_2010_p1, tm_2000_2010_p2[, 2:10], tm_2000_2010_p3[, 2:4])
colnames(tm_2000_2010) <- c('2000 (C) to 2010 (R)', tm_2000_2010[1, 2:ncol(tm_2000_2010)])
tm_2000_2010 <- tm_2000_2010[2:nrow(tm_2000_2010),]
tm_2000_2010[,1] <- colnames(tm_2000_2010)[2:ncol(tm_2000_2010)]
rm(tm_2000_2010_p1, tm_2000_2010_p2, tm_2000_2010_p3)

# Format final dataframe
      # Wide to long format
            # 1990-2000
LCchange_1990_2000 <- tm_1990_2000 %>% 
  gather(key='Source', value='CellCount', -'1990 (C) to 2000 (R)') %>%
  rename(Destination = '1990 (C) to 2000 (R)') %>%
  mutate_all(as.integer) %>%
  mutate(Source_Ownership = floor(Source/10),
         Destination_Ownership = floor(Destination/10)) %>%
  filter(Source_Ownership == Destination_Ownership) %>%
  mutate(Ownership = Source_Ownership,
         TimePeriod = '1990 - 2000',
         Source = Source %% 10,
         Destination = Destination %% 10) %>%
  select(TimePeriod, Ownership, Source, Destination, CellCount) %>%
  arrange(TimePeriod, Ownership, Source, Destination)

            # 2000-2010
LCchange_2000_2010 <- tm_2000_2010 %>% 
  gather(key='Source', value='CellCount', -'2000 (C) to 2010 (R)') %>%
  rename(Destination = '2000 (C) to 2010 (R)') %>%
  mutate_all(as.integer) %>%
  mutate(Source_Ownership = floor(Source/10),
         Destination_Ownership = floor(Destination/10)) %>%
  filter(Source_Ownership == Destination_Ownership) %>%
  mutate(Ownership = Source_Ownership,
         TimePeriod = '2000 - 2010',
         Source = Source %% 10,
         Destination = Destination %% 10) %>%
  select(TimePeriod, Ownership, Source, Destination, CellCount) %>%
  arrange(TimePeriod, Ownership, Source, Destination)

      # Bind both time periods
LCchange <- bind_rows(LCchange_1990_2000, LCchange_2000_2010)

      # Format
            # Format Land Cover Names
LC_names <- stateClassID %>%
  filter(State.Class.ID < 10) %>%
  add_row(State.Class.ID = 4, State.Class.Name = 'Forest') %>%
  arrange(State.Class.ID)

            # Add Stratum Names
LCchange$Ownership <- sapply(LCchange$Ownership, function(x) secondaryStratumID$Name[which(secondaryStratumID$Secondary.Stratum.ID == x)])
LCchange$Source <- sapply(LCchange$Source, function(x) LC_names$State.Class.Name[which(LC_names$State.Class.ID == x)])
LCchange$Destination <- sapply(LCchange$Destination, function(x) LC_names$State.Class.Name[which(LC_names$State.Class.ID == x)])

      # Add Total Area and Annual Change Rate, in ha
LCchange %<>% mutate(TotalArea_m2 = CellCount * res^2) %>%
  mutate(TotalArea_ha = TotalArea_m2/10000) %>%
  select(-c(TotalArea_m2, CellCount)) %>%
  mutate(AnnualChangeRate_ha = TotalArea_ha/10)

# Export
write.csv(LCchange, paste0(resultsDir, 'Tabular/LandCoverChange.csv'), row.names = F)
