#### a208: ECCC Olive Sided Fly Catcher Landscape Change and Habitat Model 
#### Script by Leonardo Frid

#### 9. Simulation result plots and maps for OSFL habitat change and occurence
####    probability.

library(rsyncrosim)
library(dplyr)
library(ggplot2)
library(stringr)
library(raster)
library(gridExtra)
library(tidyr)

myLib = ssimLibrary("D:/A208/ssimLibrary/DawsonTSA.ssim")
myScenarios = scenario(myLib, results = T)

# Non-Spatial Output ---------------------------------------------------------
myHabitatOutput = datasheet(myLib, name = "OutputStateAttribute", scenario = c(38,39,43,44,45,46))

#2020
ts = 2020
myOutput2020 = filter(myHabitatOutput, Timestep == ts, StateAttributeTypeID == "OSFL Habitat")
myOutput2020 = rename(myOutput2020, "Year2020" = Amount)
myOutput2020 = subset(myOutput2020, select = -c(Timestep))
#2030
ts = 2030
myOutput2030 = filter(myHabitatOutput, Timestep == ts, StateAttributeTypeID == "OSFL Habitat")
myOutput2030 = rename(myOutput2030, "Year2030" = Amount)
myOutput2030 = subset(myOutput2030, select = -c(Timestep))
#2040
ts = 2040
myOutput2040 = filter(myHabitatOutput, Timestep == ts, StateAttributeTypeID == "OSFL Habitat")
myOutput2040 = rename(myOutput2040, "Year2040" = Amount)
myOutput2040 = subset(myOutput2040, select = -c(Timestep))

#2050
ts = 2050
myOutput2050 = filter(myHabitatOutput, Timestep == ts, StateAttributeTypeID == "OSFL Habitat")
myOutput2050 = rename(myOutput2050, "Year2050" = Amount)
myOutput2050 = subset(myOutput2050, select = -c(Timestep))


myOutput = full_join(myOutput2020,myOutput2030)
myOutput = full_join(myOutput, myOutput2040)
myOutput = full_join(myOutput, myOutput2050)

myOutput$HabitatChange = myOutput$Year2050 - myOutput$Year2020

# Total Output
myOutputTotal = group_by(myOutput,ParentName, Iteration) %>% summarise(Total2020 = sum(Year2020), Total2050 = sum(Year2050))
myOutputTotal$HabitatChange = myOutputTotal$Total2050 - myOutputTotal$Total2020
myOutputTotal$HabitatChange = (myOutputTotal$HabitatChange/myOutputTotal$Total2020)*100
myOutputTotal$Management = gsub(" - Fire x 1.5", x = myOutputTotal$ParentName, replacement =  "")
myOutputTotal$Fire = str_detect(myOutputTotal$ParentName, " - Fire x 1.5", negate = FALSE)
myOutputTotal$Alpha = 1
myOutputTotal$Alpha[myOutputTotal$Fire==FALSE] = 0.9

p <- ggplot(myOutputTotal, aes(x = reorder(ParentName, HabitatChange, FUN = median), HabitatChange, fill = Management, alpha = Alpha))
p <- p + geom_boxplot() + theme_classic() + labs(fill = "Scenario")  + ylab("Habitat Change (%)") + theme(axis.title.x=element_blank(),
                                                          axis.text.x=element_blank(),
                                                          axis.ticks.x=element_blank()) + 
  theme(legend.position="none") + geom_hline(yintercept=0, linetype = "dashed") +
  guides(alpha = FALSE)

summarise(myOutputTotal, med = median(HabitatChange))

ggsave("D:/A208/Results/Plots/HabitatChange.png", device = "png", width = 8.5, height = 8.5, units = "cm", dpi = 300)
ggsave("D:/A208/Results/Plots/HabitatChange.pdf", device = "pdf", width = 8.5, height = 8.5, units = "cm", dpi = 300)

# BEC Output
myOutputTotal = group_by(myOutput,ParentName, Iteration, StratumID) %>% summarise(Total2020 = sum(Year2020), Total2050 = sum(Year2050))
myOutputTotal$HabitatChange = myOutputTotal$Total2050 - myOutputTotal$Total2020
myOutputTotal$HabitatChange = (myOutputTotal$HabitatChange/myOutputTotal$Total2020)*100
myOutputTotal = filter(myOutputTotal, StratumID != "Wetland Open")

p <- ggplot(myOutputTotal, aes(ParentName, HabitatChange, fill = ParentName))
p + geom_boxplot() + theme(legend.position="top") + theme_classic() + labs(fill = "Scenario")  + ylab("Habitat Change (%)") + theme(axis.title.x=element_blank(),
                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                    axis.ticks.x=element_blank()) +
  facet_wrap("StratumID", ncol = 3) + geom_hline(yintercept=0)

# Ownership Output
myOutputTotal = group_by(myOutput,ParentName, Iteration, SecondaryStratumID) %>% summarise(Total2020 = sum(Year2020), Total2050 = sum(Year2050))
myOutputTotal$HabitatChange = myOutputTotal$Total2050 - myOutputTotal$Total2020
myOutputTotal$HabitatChange = (myOutputTotal$HabitatChange/myOutputTotal$Total2020)*100


p <- ggplot(myOutputTotal, aes(ParentName, HabitatChange, fill = ParentName))
p + geom_boxplot() + theme(legend.position="top") + theme_classic() + labs(fill = "Scenario")  + ylab("Habitat Change (%)") + theme(axis.title.x=element_blank(),
                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                    axis.ticks.x=element_blank()) +
  facet_wrap("SecondaryStratumID", ncol = 3) + geom_hline(yintercept=0)


# Spatial Output -----------------------

myStack = datasheetRaster(myLib, "OutputSpatialStateAttribute",
                scenario = c(38,39,45,43,44,46),
                timestep = c(2020,2050),
                iteration = 1,
                subset = expression(grepl("sa_161", Filename, fixed = TRUE)))

library(RColorBrewer)
library(rasterVis)

myStack2050 = subset(myStack, c(2,4,6,8,10,12))
myStack2020 = subset(myStack, c(1))

names(myStack2050)
rasterNames  <- gsub("sa_161.it1.ts20","20", names(myStack2050))
rasterNames  <- gsub("scn38.","BAU ", rasterNames)
rasterNames  <- gsub("scn39.","Reduce Cut Size ", rasterNames)
rasterNames  <- gsub("scn45.","30% Protection ", rasterNames)
rasterNames  <- gsub("scn43.","BAU, Fire x1.5 ", rasterNames)
rasterNames  <- gsub("scn44.","Reduce Cut Size, Fire x1.5 ", rasterNames)
rasterNames  <- gsub("scn46.","30% Protection, Fire x1.5 ", rasterNames)
rasterNames  <- gsub("BAU 2020","Year 2020", rasterNames)
rasterNames  <- gsub("BAU 2050","BAU", rasterNames)
rasterNames  <- gsub("30% Protection 2050","30% Protection", rasterNames)
rasterNames  <- gsub("Reduce Cut Size 2050","Reduce Cut Size", rasterNames)
rasterNames  <- gsub("BAU, Fire x1.5 2050","", rasterNames)
rasterNames  <- gsub("Reduce Cut Size, Fire x1.5 2050","", rasterNames)
rasterNames  <- gsub("30% Protection, Fire x1.5 2050","", rasterNames)
rasterNames[5] = "Increased Fire"

cuts=c(0,0.02,0.025,0.03,0.06,0.2,0.3) #set breaks

#pal <- colorRampPalette(c("#edf8fb","#005824"))

#plot(myStack, breaks=cuts, col = pal(4)) #plot with defined breaks
cols <- colorRampPalette(brewer.pal(5,"PRGn"))
myPlot = levelplot(myStack2050, at=cuts, col.regions=cols,
          layout=c(3, 2),
          names.attr=rasterNames,
          xlab = NULL, ylab = NULL, scales=list(draw=FALSE),
          labels=list(at=cuts, labels=round(cuts, 2))) #plot with defined breaks
myPlotIC = levelplot(myStack2020, at=cuts, col.regions=cols,
                   layout=c(1, 1), colorkey=FALSE,
                   names.attr=c("2020"), margin = F,
                   labels=list(at=cuts, labels=round(cuts, 2))) #plot with defined breaks

myFinalPlot = grid.arrange(myPlotIC, myPlot, ncol=2)

ggsave("D:/A208/Results/Plots/Map.pdf", device = "pdf", width = 170, height = 8.5, units = "cm", dpi = 300)

# Initial State Class #####################################
initialStateClass = datasheetRaster(myLib, "InitialConditionsSpatial",
                          scenario = c(3),
                          column = "StateClassFileName")

legend = datasheet(myLib, project = 1, name = "StateClass")
initialStateClass <- as.factor(initialStateClass)
rat <- levels(initialStateClass)[[1]]
rat = left_join(rat, legend)

rat$Color = substring(rat$Color,5)
rat = separate(data = rat, col = Color, into = c("R", "G", "B"), sep = ",")
rat$R = as.numeric(rat$R)
rat$G = as.numeric(rat$G)
rat$B = as.numeric(rat$B)

library(ggtern)
rat$Hex = rgb2hex(rat$R, rat$G, rat$B)

plot(initialStateClass,col=rat$Hex,legend=F,box=F,axes=F)
legend(x='bottomleft', legend =rat$Name,fill = rat$Hex)

rat$level = rat$Name
rat$level = gsub(":All", "", rat$level)
levels(initialStateClass) = rat

levelplot(initialStateClass, col.regions = rat$Hex, maxpixels = 3e6)
