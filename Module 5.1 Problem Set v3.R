##### SCROLL DOWN TO THE VERY BOTTOM TO SEE PROBLEM SET


# Set wd, MUST COMMENT OUT setwd() FOR APP TO RUN

# getwd()
# setwd("C:/Users/peter/OneDrive/Documents/Athlete Lab")


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

TestTrackMan = fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))


# Start of the UI - Part 1 of App Structure

# ui <- navbarPage(
#   
#   "Title of App", theme = "flatly",
#   
#   tabPanel("Pitchers",
#            
#            sidebarLayout(
#              
#              sidebarPanel(
#                
#                selectInput("Pitcher", label = "Choose Pitcher",
#                            
#                            choices = levels(as.factor(TestTrackMan$Pitcher))),
#                width = 2),
#              
#              mainPanel(
#                
#                plotOutput("Sample Output"), width = 12, height = 12)
#            
#              ),
#            
#            ),
#   
# )


# Start of the Server - Part 2 of App Structure

# server = function(input, output, session) {
#   
#   
#   
# }


# ShinyApp - Part 3 of App Structure

# shinyApp(ui = ui, server = server)


########## Problem Set ##############

#Create the Following:

# 1) Create a structure with two tabs, "Pitchers" -- which we already have; and "Hitters"


# Start of the UI - Part 1 of App Structure

ui <- navbarPage(
  
  "Title of App", theme = "flatly",
  
  tabPanel("Pitchers",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("Pitcher", label = "Choose Pitcher",
                           
                           choices = levels(as.factor(TestTrackMan$Pitcher))),
               
               width = 2),
             
             mainPanel(
               
               plotOutput("SampleOutput"), width = 12, height = 12)
             
                ),
           
           ),
  
  tabPanel("Batters",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("Batter", label = "Choose Batter",
                           
                           choices = levels(as.factor(TestTrackMan$Batter))),
               
               width = 2),
             
             mainPanel(
               
               plotOutput("SampleOutput"), width = 12, height = 12)
                
                ),
           
           ),
  
)


# Start of the Server - Part 2 of App Structure

server = function(input, output, session) {
  
  
  
}


# ShinyApp - Part 3 of App Structure

shinyApp(ui = ui, server = server)
