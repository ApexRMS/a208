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
library(forcats)

myLib = ssimLibrary("D:/A208/DawsonTSA.ssim")
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

# Calculate mean and variation for by Scenario in 2020 ---------
myOutput2020Only <- group_by(myOutput,ParentName, Iteration) %>% 
  summarise(Total2020 = sum(Year2020))

myInitialHabitatStats <- group_by(myOutput2020Only,ParentName) %>% 
  summarise(Mean = mean(Total2020), Median = median(Total2020), LCI = quantile(Total2020, 0.025), UCI = quantile(Total2020, 0.975))

write.csv(myInitialHabitatStats, "InitialHabitatStats.csv")


# Create a plot of Habitat Change --------------------------------
myOutput$HabitatChange = myOutput$Year2050 - myOutput$Year2020

# Define manual orders for factors
orderParentName <- c("Business as Usual - Fire x 1.5",
                     "Business as Usual",
                     "Increased Protected Area - Fire x 1.5",
                     "Increased Protected Area",
                     "Reduced Cutblock Size - Fire x 1.5",
                     "Reduced Cutblock Size")
orderFire <- c("Increased Fire", "Baseline Fire")

# Total Output

myOutputTotal <- group_by(myOutput,ParentName, Iteration) %>% 
  summarise(Total2020 = sum(Year2020), Total2050 = sum(Year2050)) %>%
  mutate(
    HabitatChange = (Total2050 - Total2020) / Total2020 * 100,        # Change as a percentage
    ParentName =    str_replace(ParentName, "Increase", "Increased"), # For consistency with S2, S3, Fig 4
    ParentName =    str_replace(ParentName, "Reduce Clearcut", "Reduced Cutblock"), # For consistency with S2, S3, Fig 4
    Management = str_replace(ParentName, " - Fire x 1.5", ""),
    Fire = str_detect(ParentName, "Fire") %>% ifelse("Increased Fire", "Baseline Fire"),
    # Recode as factors and reorder manually
    ParentName = factor(ParentName, levels = orderParentName),
    Fire = factor(Fire, levels = orderFire)
  )

p <- myOutputTotal %>%
  ggplot(aes(x = ParentName, y = HabitatChange, fill = Management, alpha = Fire)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(fill = "Scenario", alpha = "", y = "Habitat Change (%)") +
    scale_alpha_manual(values = c(1, 0.4)) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      #legend.position = "none"
    ) +
    guides(
      fill = guide_legend(order = 1),
      alpha = guide_legend(override.aes = list(fill = "grey50", alpha = c(1, 0.4)))
    )

save_plot("D:/A208/Results/Plots/HabitatChange.pdf", p, base_width = 6, base_height = 4)

# BEC Output
myOutputTotalBEC <- group_by(myOutput,ParentName, Iteration, StratumID) %>% 
  summarise(Total2020 = sum(Year2020), Total2050 = sum(Year2050)) %>%
  filter(StratumID != "Wetland Open") %>%
  mutate(
    HabitatChange = (Total2050 - Total2020) / Total2020 * 100,        # Change as a percentage
    ParentName =    str_replace(ParentName, "Increase", "Increased"), # For consistency with S2, S3, Fig 4
    ParentName =    str_replace(ParentName, "Reduce Clearcut", "Reduced Cutblock"), # For consistency with S2, S3, Fig 4
    Management = str_replace(ParentName, " - Fire x 1.5", ""),
    Fire = str_detect(ParentName, "Fire") %>% ifelse("Increased Fire", "Baseline Fire"),
    # Recode as factors and reorder manually
    ParentName = factor(ParentName, levels = orderParentName),
    Fire = factor(Fire, levels = orderFire)
  )

pBEC <- myOutputTotalBEC %>%
  ggplot(aes(x = ParentName, y = HabitatChange, fill = Management, alpha = Fire)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.8) + # Thinner boxplot lines to make fills more visible
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ StratumID) +
  labs(fill = "Scenario", alpha = "", y = "Habitat Change (%)") +
  scale_alpha_manual(values = c(1, 0.4)) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    #legend.position = "none"
  ) +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(override.aes = list(fill = "grey50", alpha = c(1, 0.4)))
  )

save_plot("D:/A208/Results/Plots/S2-BEC.pdf", pBEC, base_width = 6, base_height = 4.5)

# Ownership Output (Owner)
myOutputTotalOwner <- group_by(myOutput,ParentName, Iteration, SecondaryStratumID) %>% 
  summarise(Total2020 = sum(Year2020), Total2050 = sum(Year2050)) %>%
  mutate(
    HabitatChange = (Total2050 - Total2020) / Total2020 * 100,        # Change as a percentage
    ParentName =    str_replace(ParentName, "Increase", "Increased"), # For consistency with S2, S3, Fig 4
    ParentName =    str_replace(ParentName, "Reduce Clearcut", "Reduced Cutblock"), # For consistency with S2, S3, Fig 4
    Management = str_replace(ParentName, " - Fire x 1.5", ""),
    Fire = str_detect(ParentName, "Fire") %>% ifelse("Increased Fire", "Baseline Fire"),
    # Recode as factors and reorder manually
    ParentName = factor(ParentName, levels = orderParentName),
    Fire = factor(Fire, levels = orderFire)
  )

pOwner <- myOutputTotalOwner %>%
  ggplot(aes(x = ParentName, y = HabitatChange, fill = Management, alpha = Fire)) +
  geom_boxplot(lwd = 0.1) + # Thinner boxplot lines to make fills more visible
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ SecondaryStratumID) +
  labs(fill = "Scenario", alpha = "", y = "Habitat Change (%)") +
  scale_alpha_manual(values = c(1, 0.4)) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    #legend.position = "none"
  ) +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(override.aes = list(fill = "grey50", alpha = c(1, 0.4)))
  )

save_plot("D:/A208/Results/Plots/S3-Ownership.pdf", pOwner, base_width = 6, base_height = 3)

# Cumulative area burned and cut --------------------------------------------

transitionArea <- datasheet(myLib, 
                            name = "OutputStratumTransition", 
                            scenario = c(38,39,43,44,45,46), 
                            fastQuery = T)

clearCutBurnArea <-filter(transitionArea, 
                          TransitionGroupID %in% 
                            c("Disturbance: Clearcut [Type]", 
                              "Disturbance: Fire [Type]"))


myClearCutBurnTotal <- group_by(clearCutBurnArea,ParentName, Iteration) %>% 
  summarise(Total = sum(Amount))

myClearCutBurnStats <- group_by(myClearCutBurnTotal,ParentName) %>% 
  summarise(Mean = mean(Total), Median = median(Total), LCI = quantile(Total, 0.025), UCI = quantile(Total, 0.975))

write.csv(myClearCutBurnStats, "clearcutBurnStats.csv")

# Spatial Output -----------------------

myStack = datasheetRaster(myLib, "OutputSpatialStateAttribute",
                scenario = c(38,39,45,43,44,46),
                timestep = c(2020,2050),
                iteration = 1,
                subset = expression(grepl("sa_161", Filename, fixed = TRUE)))

library(RColorBrewer)
library(rasterVis)
library(cowplot)

myStack2050 = subset(myStack, c(2,6,4,8,12,10))
myStack2020 = subset(myStack, c(1))

rasterNames <- names(myStack2050) %>%
  str_replace("sa_161.it1.ts20","20") %>%                   # Remove time step, iteration info
  str_replace("scn38.","Business as Usual ") %>%
  str_replace("scn39.","Reduced Cutblock Size ") %>%
  str_replace("scn45.","Increased Protected Areas ") %>%
  str_replace("scn43.","Business as Usual, Fire x1.5 ") %>%
  str_replace("scn44.","Reduced Cutblock Size, Fire x1.5 ") %>%
  str_replace("scn46.","Increased Protected Areas, Fire x1.5 ")

cuts=c(0,0.02,0.025,0.03,0.06,0.2,0.3) #set breaks

#pal <- colorRampPalette(c("#edf8fb","#005824"))

#plot(myStack, breaks=cuts, col = pal(4)) #plot with defined breaks
cols <- colorRampPalette(brewer.pal(5,"PRGn"))
myPlot = levelplot(myStack2050, at=cuts, col.regions=cols,
          layout=c(3, 2),
          names.attr= rep("", 6), # Replace `rep(...)` with `rasterNames` to ensure plots are labeled correctly further down
          xlab = NULL, ylab = NULL, scales=list(draw=FALSE),
          labels=list(at=cuts, labels=round(cuts, 2))) # plot with defined breaks
myPlotIC = levelplot(myStack2020, at=cuts, col.regions=cols,
                   layout=c(1, 1), colorkey=FALSE,
                   names.attr=c("2020"), margin = F,
                   labels=list(at=cuts, labels=round(cuts, 2))) #plot with defined breaks

annotatedGrid <- ggdraw(myPlot) +
  draw_label("Business\nas Usual",         x = 0.18, y = 0.95, size = 12) +
  draw_label("Increased\nProtected Areas", x = 0.44, y = 0.95, size = 12) +
  draw_label("Reduced\nCutblock Size",     x = 0.69, y = 0.95, size = 12) +
  draw_label("Baseline Fire",  angle = 90, x = 0.04, y = 0.69, size = 12) +
  draw_label("Increased Fire", angle = 90, x = 0.04, y = 0.25, size = 12)

S4 <- plot_grid(
  myPlotIC,
  annotatedGrid,
  rel_widths = c(5, 7.3),
  labels = c("Occurrence Probability 2020", "Occurrence Probability 2050"),
  label_size = 12,
  label_x = c(-0.08, 0),
  label_y = 1.1
)

S4Padded <- plot_grid(
  NULL, #Empty space above
  S4,
  rel_heights = c(0.1, 1),
  nrow = 2
)

save_plot("D:/A208/Results/Plots/S4-Map.pdf", S4Padded, base_height = 4, base_width = 8, dpi = 450)

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
