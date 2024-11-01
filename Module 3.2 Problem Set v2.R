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


# Import data / CSV & Remove Extra Column "V1"

TestTrackMan=fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))



# Create LHP / RHP Split Tables

LHP <- TestTrackMan[TestTrackMan$PitcherThrows == "Left"]
RHP <- TestTrackMan[TestTrackMan$PitcherThrows == "Right"]

# rbind -- LHP & RHP -- has to have the same columns

Total <- rbind(LHP, RHP)





# Create Team and Throws Tables

LHP_Velo <- LHP[, .(
  MPH = mean(RelSpeed, na.rm = TRUE)),
  by=.(PitcherThrows)]

RHP_Velo <- RHP[, .(
  MPH = mean(RelSpeed, na.rm = TRUE)),
  by=.(PitcherThrows)]



# cbind

Velos <- cbind(LHP_Velo, RHP_Velo)





# Create Team and Throws Table

Team <- subset(TestTrackMan, select = c(Pitcher, PitcherTeam))
Throws <- subset(TestTrackMan, select = c(Pitcher, PitcherThrows))

Team <- head(Team, 1)
Throws <- head(Throws, 1)

# merge

Team_Throws <- merge(Team, Throws)









########## Problem Set ##############

#Create the Following:

# 1) Combine PitcherData with TestTrackMan to add the following
#    column: "is.fastball" - in conjunction with the previous session

# Read in data and remove column X

TestTrackMan <- read.csv("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(X))


# Create PitcherData using Pitcher, TaggedPitchType & RelSpeed columns

PitcherData <- subset(TestTrackMan, select = c(Pitcher, TaggedPitchType, RelSpeed))

# Add is.fastball column

PitcherData$is.fastball <- PitcherData$TaggedPitchType

# Display unique TaggedPitchType

unique(PitcherData$TaggedPitchType)

# Change pitch types to either "yes" or "no"

PitcherData$is.fastball[PitcherData$is.fastball == "Fastball"] <- "yes"
PitcherData$is.fastball[PitcherData$is.fastball == "Sinker"] <- "yes"
PitcherData$is.fastball[PitcherData$is.fastball == "ChangeUp"] <- "no"
PitcherData$is.fastball[PitcherData$is.fastball == "Slider"] <- "no"
PitcherData$is.fastball[PitcherData$is.fastball == "Curveball"] <- "no"

# Confirm changes in is.fastball column

unique(PitcherData$is.fastball)


# Merge the original data set with PitcherData

MergedSet <- merge(TestTrackMan, PitcherData)
