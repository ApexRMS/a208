#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Chloé Debyser

#### 6. Logging History Analysis

############################################################################################################################
# This code:                                                                                                               #
# 1. Creates a LoggingHistoryAnalysis GRASS mapset                                                                         #
# 2. Imports Primary Stratum, Secondary Stratum, and CUTBLOCKS data to GRASS mapset                                        #
# 3. Imports study region clipping mask to GRASS mapset                                                                    #
# 4. Computes area clearcut per year, per Primary Stratum, and per Secondary Stratum                                       #
# 5. Determines the clearcut size class distribution                                                                       #
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
subzoneID <- read.xlsx(paste0(tabularDataDir, "BEC Subzone.xlsx"), sheet="Stratum") %>% # Load
  mutate(Name = ifelse(Name == 'SBSwc', 'SBSwk', ifelse(Name == 'ESSFmv2', 'ESSFmv', ifelse(Name == 'BWBSwk1', 'BWBSwk', Name)))) # Correct errors in .xlsx file
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
# Initiate GRASS in 'PrepareStateClass' mapset
initGRASS(gisBase=gisBase, gisDbase=gisDbase, location='a208', mapset='FireHistoryAnalysis')

# Initialize new mapset inheriting projection info
execGRASS("g.mapset", mapset = "LoggingHistoryAnalysis", flags="c")

# Import data
execGRASS('g.copy', raster=c('primaryStratum@PreparePrimaryStratum', 'primaryStratum'))
execGRASS('g.copy', raster=c('secondaryStratum@PrepareSecondaryStratum', 'secondaryStratum'))
execGRASS('g.copy', vector=c('rawData_CUTBLOCKS@PrepareStateClass', 'rawData_CUTBLOCKS'))
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

#### Logging History Analysis ####
# Polygonize Primary Stratum raster
execGRASS('r.to.vect', input='primaryStratum', output='primaryStratum_vect', type='area', column='Stratum')

# Polygonize Secondary Stratum raster
execGRASS('r.to.vect', input='secondaryStratum', output='secondaryStratum_vect', type='area', column='Stratum')

# Union Stratum and CUTBLOCKS data
execGRASS('v.overlay', ainput='rawData_CUTBLOCKS', binput="primaryStratum_vect", operator="or", output='PrimaryStratumUnionCutblocks')
execGRASS('v.overlay', ainput='PrimaryStratumUnionCutblocks', binput="secondaryStratum_vect", operator="or", output='StratumUnionCutblocks')

# Area clearcut per stratum | year
      # Compute area of each polygon
execGRASS('v.db.addcolumn', map='StratumUnionCutblocks', columns='Area double precision')
execGRASS('v.to.db', map='StratumUnionCutblocks', columns='Area', option='area', units='hectares')

      # Get attribute table
att_StratumUnionCutblocks <- v.get.att("StratumUnionCutblocks", "&")

      # Create template stratum | year dataframe
strata <- att_StratumUnionCutblocks %>%
  drop_na(a_b_cat) %>% # Remove areas that are not part of the Primary/Secondary Stratum
  rename(PrimaryStratum = a_b_Stratum, SecondaryStratum = b_Stratum) %>%
  select(PrimaryStratum, SecondaryStratum) %>%
  arrange(PrimaryStratum, SecondaryStratum) %>%
  distinct()

years <- min(att_StratumUnionCutblocks$a_a_HARVEST_YE, na.rm=T):max(att_StratumUnionCutblocks$a_a_HARVEST_YE, na.rm=T)

template <- data.frame(PrimaryStratum = rep(strata$PrimaryStratum, times=length(years)),
                       SecondaryStratum = rep(strata$SecondaryStratum, times=length(years)),
                       Year = rep(years, each=nrow(strata)))

      # Compute area clearcut per stratum | year
areaCut_Stratum_Year <- att_StratumUnionCutblocks %>%
  drop_na(a_b_cat) %>% # Remove areas that are not part of the Primary/Secondary Stratum
  drop_na(a_a_cat) %>% # Remove areas that were not cut
  rename(PrimaryStratum = a_b_Stratum, SecondaryStratum = b_Stratum, Year = a_a_HARVEST_YE) %>%
  group_by(PrimaryStratum, SecondaryStratum, Year) %>%
  summarize(AreaCut_ha = sum(Area)) %>%
  arrange(PrimaryStratum, SecondaryStratum, Year) %>%
  ungroup()

      # Populate template database
areaCut_Stratum_Year %<>% left_join(template, .) %>%
  mutate(AreaCut_ha = replace_na(AreaCut_ha, 0)) %>%
  arrange(PrimaryStratum, SecondaryStratum, Year)

      # Change Stratum ID to Stratum Name
areaCut_Stratum_Year$PrimaryStratum <- sapply(areaCut_Stratum_Year$PrimaryStratum, function(x) subzoneID$Name[which(subzoneID$ID == x)])
areaCut_Stratum_Year$SecondaryStratum <- sapply(areaCut_Stratum_Year$SecondaryStratum, function(x) secondaryStratumID$Name[which(secondaryStratumID$Secondary.Stratum.ID == x)])

      # Export
write.xlsx(areaCut_Stratum_Year, paste0(resultsDir, 'Tabular/AreaCut_ByStratum_ByYear.xlsx'))

# Clearcut count by clearcut size class
      # Select CUTBLOCKS that overlap study area
execGRASS("v.select", ainput='rawData_CUTBLOCKS', binput='primaryStratum_vect', output='CUTBLOCKS_area_inter', operator='overlap', 'overwrite')

      # Remove clearcuts that are older than year 2000
execGRASS('v.extract', input='CUTBLOCKS_area_inter', where='HARVEST_YE >= 2000', output='CUTBLOCKS_area')
execGRASS('g.remove', type='vector', name='CUTBLOCKS_area_inter', 'f')

      # Multipart to singlepart
execGRASS('v.db.droptable', map='CUTBLOCKS_area', 'f')
execGRASS('v.category', input='CUTBLOCKS_area', output='CUTBLOCKS_area_singlePart_inter', option='del', cat=-1, 'overwrite')
execGRASS('v.category', input='CUTBLOCKS_area_singlePart_inter', output='CUTBLOCKS_area_singlePart', option='add', 'overwrite')
execGRASS('g.remove', type='vector', name='CUTBLOCKS_area_singlePart_inter', 'f')

      # Compute area
execGRASS('v.db.addtable', map='CUTBLOCKS_area_singlePart')
execGRASS('v.db.addcolumn', map='CUTBLOCKS_area_singlePart', columns='Area double precision')
execGRASS('v.to.db', map='CUTBLOCKS_area_singlePart', columns='Area', option='area', units='hectares')

      # Get attribute table
att_CUTBLOCKS <- v.get.att("CUTBLOCKS_area_singlePart", "&")

      # Get fire sizes in ha
cutSizes <- att_CUTBLOCKS %>%
  rename(Area_ha = Area) %>%
  select(Area_ha) %>%
  arrange(Area_ha)

      # Define size classes
interval <- 50 # Set interval size
mins <- seq(from=floor(min(cutSizes)), by=interval, length.out=ceiling(max(cutSizes)/interval))
maxs <- mins+interval
  
      # Compute fire count by fire size class
cutCount_SizeClass <- data.frame(SizeClass_Min_ha = mins,
                                 SizeClass_Max_ha = maxs)
cutCount_SizeClass$ClearcutCount <- sapply(1:nrow(cutCount_SizeClass),
                                        function(x) length(cutSizes[which((cutSizes$Area_ha >= cutCount_SizeClass$SizeClass_Min_ha[x]) & (cutSizes$Area_ha < cutCount_SizeClass$SizeClass_Max_ha[x])),]))

      # Export
write.csv(cutSizes, paste0(resultsDir, "Tabular/CutSizes.csv"), row.names=F)
write.csv(cutCount_SizeClass, paste0(resultsDir, "Tabular/CutCount_bySizeClass.csv"), row.names=F)
