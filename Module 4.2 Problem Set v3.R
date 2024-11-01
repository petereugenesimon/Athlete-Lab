##### SCROLL DOWN TO THE VERY BOTTOM TO SEE PROBLEM SET


getwd()
setwd("C:/Users/peter/OneDrive/Documents/Athlete Lab")


# load packages

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(ggpubr)


# Import data / CSV & Remove Extra Column "V1"

TestTrackMan = fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))


# Filtering Data by Column Value -- Pitcher Name

unique(TestTrackMan$Pitcher)


# Strike Zone Dimensions

Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3


# Larry

# Filter TestTrackMan for Larry

Larry <- TestTrackMan%>%
  filter(Pitcher == "Foster, Larry")%>%
  
  # Pitches Plot
  
  ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType), size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green", Sinker = "grey",
                                Splitter = "purple")) +
  
  # Plot Strike Zone
  
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  # Format Plot
  
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) + ggtitle("Larry")


# Carson

# Filter TestTrackMan for Carson

Carson <- TestTrackMan%>%
  filter(Pitcher == "Sentz, Carson")%>%
  
  # Pitches Plot
  
  ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType), size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green", Sinker = "grey",
                                Splitter = "purple")) +
  
  # Plot Strike Zone
  
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  # Format Plot
  
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  xlim(-2.5, 2.5) + ylim(-.5, 5) + ggtitle("Carson")


# Combining Both Graphs

ggarrange(Larry, Carson, nrow = 1, ncol = 2)


# Line Graph - Velo Per Pitch; by Pitcher, TaggedPitchType

# Filter TestTrackMan for Larry

TestTrackMan%>%
  filter(Pitcher == "Foster, Larry")%>%
  
  # Plot Velo
  
  ggplot(TestTrackMan, mapping = aes(x = PitchNo, y = RelSpeed, colour = TaggedPitchType)) +
  geom_line() + geom_point() +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green", Sinker = "grey",
                                Splitter = "purple")) +
  
  # Format Plot
  
  ggtitle("Velocity / Pitch") +
  xlab("Pitch") + ylab("Velocity") + theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "black", size = 13, face = "bold"),
    axis.title.y = element_text(color = "black", size = 13, face = "bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  geom_hline(yintercept = seq(from = 70, to = 100, by = 5))


# Percentiles - Max Fastball Velo & Spin; by Pitcher (and TaggedPitchType == "Fastball")

# Finding Max Velo & Max SpinRate for Each Pitcher by Pitch Type

TM_Percentiles <- TestTrackMan[, .(
  'Max Velo' = max(RelSpeed, na.rm = TRUE),
  'Max Spin' = max(SpinRate, na.rm = TRUE)),
  by = .(Pitcher, TaggedPitchType)]

# Rounding to Single Decimal Point

TM_Percentiles$'Max Velo' <- round(TM_Percentiles$'Max Velo', digits = 1)
TM_Percentiles$'Max Spin' <- round(TM_Percentiles$'Max Spin', digits = 0)

# Creating Ranking and Percentile for Max Velo -- Fastball Only!

# Create MaxVelo_Percentile Column and Rounding

TM_Percentiles$MaxVelo_Percentile <- TM_Percentiles$'Max Velo'
TM_Percentiles$MaxVelo_Percentile <- round(TM_Percentiles$MaxVelo_Percentile, digits = 2)

# Substituting NA for non-fastball pitches

TM_Percentiles$MaxVelo_Percentile[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Create MaxVelo_Ranking Column and Ordering

TM_Percentiles$MaxVelo_Ranking[order(TM_Percentiles$MaxVelo_Percentile,
                                     decreasing = TRUE)] <- 1:nrow(TM_Percentiles)

# Substituting NA for non-fastball pitches

TM_Percentiles$MaxVelo_Ranking[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Refining MaxVelo_Percentile column based on MaxVelo_Ranking and Rounding

TM_Percentiles$MaxVelo_Percentile <- 1 - ((TM_Percentiles$MaxVelo_Ranking) / max(TM_Percentiles$MaxVelo_Ranking, na.rm = TRUE))
TM_Percentiles$MaxVelo_Percentile <- round(TM_Percentiles$MaxVelo_Percentile, digits = 2)
TM_Percentiles$MaxVelo_Percentile <- TM_Percentiles$MaxVelo_Percentile * 100


# Creating Ranking and Percentile for Max Spin -- Fastball Only!

# Create MaxSpin_Percentile Column and Rounding

TM_Percentiles$MaxSpin_Percentile <- TM_Percentiles$'Max Spin'
TM_Percentiles$MaxSpin_Percentile <- round(TM_Percentiles$MaxSpin_Percentile, digits = 2)

# Substituting NA for non-fastball pitches

TM_Percentiles$MaxSpin_Percentile[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Create MaxSpin_Ranking Column and Ordering
TM_Percentiles$MaxSpin_Ranking[order(TM_Percentiles$MaxSpin_Percentile,
                                     decreasing = TRUE)] <- 1:nrow(TM_Percentiles)

# Substituting NA for non-fastball pitches

TM_Percentiles$MaxSpin_Ranking[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Refining MaxSpin_Percentile column based on MaxSpin_Ranking and Rounding

TM_Percentiles$MaxSpin_Percentile <- 1 - ((TM_Percentiles$MaxSpin_Ranking) / max(TM_Percentiles$MaxSpin_Ranking, na.rm = TRUE))
TM_Percentiles$MaxSpin_Percentile <- round(TM_Percentiles$MaxSpin_Percentile, digits = 2)
TM_Percentiles$MaxSpin_Percentile <- TM_Percentiles$MaxSpin_Percentile * 100


# Removing NA rows (non-fastball)

TM_Percentiles <-TM_Percentiles[!is.na(TM_Percentiles$MaxVelo_Percentile),]


# Creating High and Low Pitches for Color Scale

# Low

# Select pitchers and remove any duplicates

All_Data_Low = subset(TM_Percentiles, select = c(1))
All_Data_Low <- All_Data_Low[!duplicated(All_Data_Low)]

# Create Low Pitch

All_Data_Low$TaggedPitchType <- "Low"
All_Data_Low$'Max Velo' <- 0
All_Data_Low$'Max Spin' <- 0
All_Data_Low$MaxVelo_Percentile <- -5
All_Data_Low$MaxVelo_Ranking <- 20
All_Data_Low$MaxSpin_Percentile <- -5
All_Data_Low$MaxSpin_Ranking <- 20


# High

# Select pitchers and remove any duplicates

All_Data_High = subset(TM_Percentiles, select = c(1))
All_Data_High <- All_Data_High[!duplicated(All_Data_High)]

# Create High Pitch

All_Data_High$TaggedPitchType <- "High"
All_Data_High$'Max Velo' <- 100
All_Data_High$'Max Spin' <- 100
All_Data_High$MaxVelo_Percentile <- 105
All_Data_High$MaxVelo_Ranking <- 1
All_Data_High$MaxSpin_Percentile <- 105
All_Data_High$MaxSpin_Ranking <- 1


# Rbind to Combine Low and High

LowHigh <- rbind(All_Data_Low, All_Data_High)


# Rbind to Combine LowHigh with TM_Percentiles

TM_Percentiles <- rbind(TM_Percentiles, LowHigh)

# Export TM_Percentiles

write.csv(TM_Percentiles, "TM_Percentiles.csv")


# FB Max Velo

# Filter TM_Percentiles for Larry

MaxVelo <- TM_Percentiles %>%
  filter(Pitcher == "Foster, Larry",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  
  # Plot Percentiles
  
  ggplot(TM_Percentiles, mapping = aes(x = MaxVelo_Percentile, y = TaggedPitchType, colour = (MaxVelo_Percentile))) +
  geom_line() + geom_point(size = 9) +
  
  # Format Plot
  
  ggtitle("Max Velo") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "italic", colour = "black")) +
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxVelo_Percentile, y = TaggedPitchType, fill = MaxVelo_Percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label = MaxVelo_Percentile), hjust = .5, vjust = .4, color = "black",
            size = 5) + theme(legend.position = "none") +
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000", na.value = "grey50") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())


# FB Max Spin

# Filter TM_Percentiles for Larry

MaxSpin <- TM_Percentiles %>%
  filter(Pitcher == "Foster, Larry",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  
  # Plot Percentiles
  
  ggplot(TM_Percentiles, mapping = aes(x = MaxSpin_Percentile, y = TaggedPitchType, colour = (MaxSpin_Percentile))) +
  geom_line() + geom_point(size = 9) +
  
  # Format Plot
  
  ggtitle("Max Spin") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "italic", colour = "black")) +
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxSpin_Percentile, y = TaggedPitchType, fill = MaxSpin_Percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label = MaxSpin_Percentile), hjust = .5, vjust = .4, color = "black",
            size = 5) + theme(legend.position = "none") +
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000", na.value = "grey50") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())


# Show plot

ggarrange(MaxVelo, MaxSpin, nrow = 1, ncol = 2)
             

########## Problem Set ##############

#Create the Following:

# 1) Create a new ggarrange, but this time a 3x3 with either: different metrics OR different pitchers/hitters

# Import TestTrackMan data

TestTrackMan = fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))


# Percentiles - max extension, vertical approach angle & induced vertical break for fastballs

# Finding max ext., VAA & IVB for each pitcher by pitch type

TM3_Percentiles <- TestTrackMan[, .(
  'Max Ext' = max(Extension, na.rm = TRUE),
  'Max VAA' = max(VertApprAngle, na.rm = TRUE),
  # MAX for IVB for fastballs as positive IVB for fastballs is better
  'Max IVB' = max(InducedVertBreak, na.rm = TRUE)),
  by = .(Pitcher, TaggedPitchType)]

# Rounding to single decimal point

TM3_Percentiles$'Max Ext' <- round(TM3_Percentiles$'Max Ext', digits = 1)
TM3_Percentiles$'Max VAA' <- round(TM3_Percentiles$'Max VAA', digits = 1)
TM3_Percentiles$'Max IVB' <- round(TM3_Percentiles$'Max IVB', digits = 1)


# Creating ranking and percentile for max ext.

TM3_Percentiles$MaxExt_Percentile <- TM3_Percentiles$'Max Ext'

# Rounding

TM3_Percentiles$MaxExt_Percentile <- round(TM3_Percentiles$MaxExt_Percentile, digits = 2)

# Subbing NA for non-fastballs

TM3_Percentiles$MaxExt_Percentile[TM3_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Ranking percentiles

TM3_Percentiles$MaxExt_Ranking[order(TM3_Percentiles$MaxExt_Percentile,
                                     decreasing = TRUE)] <- 1:nrow(TM3_Percentiles)

# Subbing NA for non-fastballs

TM3_Percentiles$MaxExt_Ranking[TM3_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Calcing actual percentile value

TM3_Percentiles$MaxExt_Percentile <- 1 - ((TM3_Percentiles$MaxExt_Ranking) /
                                            max(TM3_Percentiles$MaxExt_Ranking, na.rm = TRUE))

# Rounding and cleaning up percentile value

TM3_Percentiles$MaxExt_Percentile <- round(TM3_Percentiles$MaxExt_Percentile, digits = 2)
TM3_Percentiles$MaxExt_Percentile <- TM3_Percentiles$MaxExt_Percentile * 100


# Creating ranking and percentile for max VAA

TM3_Percentiles$MaxVAA_Percentile <- TM3_Percentiles$'Max VAA'

# Rounding

TM3_Percentiles$MaxVAA_Percentile <- round(TM3_Percentiles$MaxVAA_Percentile, digits = 2)

# Subbing NA for non-fastballs

TM3_Percentiles$MaxVAA_Percentile[TM3_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Ranking percentiles

TM3_Percentiles$MaxVAA_Ranking[order(TM3_Percentiles$MaxVAA_Percentile,
                                     decreasing = TRUE)] <- 1:nrow(TM3_Percentiles)

# Subbing NA for non-fastballs

TM3_Percentiles$MaxVAA_Ranking[TM3_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Calcing actual percentile value

TM3_Percentiles$MaxVAA_Percentile <- 1 - ((TM3_Percentiles$MaxVAA_Ranking) /
                                            max(TM3_Percentiles$MaxVAA_Ranking, na.rm = TRUE))

# Rounding and cleaning up percentile value

TM3_Percentiles$MaxVAA_Percentile <- round(TM3_Percentiles$MaxVAA_Percentile, digits = 2)
TM3_Percentiles$MaxVAA_Percentile <- TM3_Percentiles$MaxVAA_Percentile * 100


# creating ranking and percentile for max IVB

TM3_Percentiles$MaxIVB_Percentile <- TM3_Percentiles$'Max IVB'

# Rounding

TM3_Percentiles$MaxIVB_Percentile <- round(TM3_Percentiles$MaxIVB_Percentile, digits = 2)

# Subbing NA for non-fastballs

TM3_Percentiles$MaxIVB_Percentile[TM3_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Ranking percentiles

TM3_Percentiles$MaxIVB_Ranking[order(TM3_Percentiles$MaxIVB_Percentile,
                                       decreasing = TRUE)] <- 1:nrow(TM3_Percentiles)

# Subbing NA for non-fastballs

TM3_Percentiles$MaxIVB_Ranking[TM3_Percentiles$TaggedPitchType != "Fastball"] <- NA

# Calcing actual percentile value

TM3_Percentiles$MaxIVB_Percentile <- 1 - ((TM3_Percentiles$MaxIVB_Ranking) /
                                              max(TM3_Percentiles$MaxIVB_Ranking, na.rm = TRUE))

# Rounding and cleaning up percentile value

TM3_Percentiles$MaxIVB_Percentile <- round(TM3_Percentiles$MaxIVB_Percentile, digits = 2)
TM3_Percentiles$MaxIVB_Percentile <- TM3_Percentiles$MaxIVB_Percentile * 100


# Removing NA rows (non-fastballs)

TM3_Percentiles <- TM3_Percentiles[!is.na(TM3_Percentiles$MaxExt_Percentile), ]


# Creating high and low pitches for color scale

# Low

All_Data_Low3 = subset(TM3_Percentiles, select = c(1))

All_Data_Low3 <- All_Data_Low3[!duplicated(All_Data_Low3)]

All_Data_Low3$TaggedPitchType <- "Low"

All_Data_Low3$'Max Ext' <- 0

All_Data_Low3$'Max VAA' <- 0

All_Data_Low3$'Max IVB' <- 0

All_Data_Low3$'MaxExt_Percentile' <- -5

All_Data_Low3$'MaxExt_Ranking' <- 20

All_Data_Low3$'MaxVAA_Percentile' <- -5

All_Data_Low3$'MaxVAA_Ranking' <- 20

All_Data_Low3$'MaxIVB_Percentile' <- -5

All_Data_Low3$'MaxIVB_Ranking' <- 20

# High

All_Data_High3 = subset(TM3_Percentiles, select = c(1))

All_Data_High3 <- All_Data_High3[!duplicated(All_Data_High3)]

All_Data_High3$TaggedPitchType <- "High"

All_Data_High3$'Max Ext' <- 100

All_Data_High3$'Max VAA' <- 100

All_Data_High3$'Max IVB' <- 100

All_Data_High3$'MaxExt_Percentile' <- 105

All_Data_High3$'MaxExt_Ranking' <- 1

All_Data_High3$'MaxVAA_Percentile' <- 105

All_Data_High3$'MaxVAA_Ranking' <- 1

All_Data_High3$'MaxIVB_Percentile' <- 105

All_Data_High3$'MaxIVB_Ranking' <- 1


# Rbind to combine High & Low

LowHigh3 <- rbind(All_Data_Low3, All_Data_High3)


# Rbind to combine LowHigh & Percentiles

TM3_Percentiles <- rbind(TM3_Percentiles, LowHigh3)


# Create sliders

# Max Extension (Fastball)

# Filter for Larry Foster

MaxExt <- TM3_Percentiles %>%
  filter(Pitcher == "Foster, Larry",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  
  # Create plot
  
  ggplot(TM3_Percentiles, mapping = aes(x = MaxExt_Percentile, y = TaggedPitchType, colour = (MaxExt_Percentile))) +
  geom_line() + geom_point(size = 9) +
  ggtitle("Max Extension") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "italic", colour = "black")) +
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxExt_Percentile, y = TaggedPitchType, fill = MaxExt_Percentile), pch = 21,
             color = "black", size = 10) +
  
  # Format plot
  
  geom_text(aes(label = MaxExt_Percentile), hjust = .5, vjust = .4, color = "black",
            size = 5) + theme(legend.position = "none") +
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000",
                       na.value = "grey50") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


# Max VAA (Fastball)

# Filter for Larry Foster

MaxVAA <- TM3_Percentiles %>%
  filter(Pitcher == "Foster, Larry",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  
  # Create plot
  
  ggplot(TM3_Percentiles, mapping = aes(x = MaxVAA_Percentile, y = TaggedPitchType, colour = (MaxVAA_Percentile))) +
  geom_line() + geom_point(size = 9) +
  ggtitle("Max VAA") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "italic", colour = "black")) +
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxVAA_Percentile, y = TaggedPitchType, fill = MaxVAA_Percentile), pch = 21,
             color = "black", size = 10) +
  
  # Format plot
  
  geom_text(aes(label = MaxVAA_Percentile), hjust = .5, vjust = .4, color = "black",
            size = 5) + theme(legend.position = "none") +
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000",
                       na.value = "grey50") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


# Max VAA (Fastball)

# Filter for Larry Foster

MaxIVB <- TM3_Percentiles %>%
  filter(Pitcher == "Foster, Larry",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  
  # Create plot
  
  ggplot(TM3_Percentiles, mapping = aes(x = MaxIVB_Percentile, y = TaggedPitchType, colour = (MaxIVB_Percentile))) +
  geom_line() + geom_point(size = 9) +
  ggtitle("Max IVB") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "italic", colour = "black")) +
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxIVB_Percentile, y = TaggedPitchType, fill = MaxIVB_Percentile), pch = 21,
             color = "black", size = 10) +
  
  # Format plot
  
  geom_text(aes(label = MaxIVB_Percentile), hjust = .5, vjust = .4, color = "black",
            size = 5) + theme(legend.position = "none") +
  scale_fill_gradient2(midpoint = 50, low = "#2952a3", mid = "#ffffff", high = "#cc0000",
                       na.value = "grey50") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


# Plot sliders

ggarrange(MaxExt, MaxVAA, MaxIVB, nrow = 1, ncol = 3)

# Plot sliders with different arrangement

ggarrange(MaxExt, MaxVAA, MaxIVB, nrow = 3, ncol = 1)
