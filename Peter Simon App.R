# Get wd() and setwd(), COMMENTED OUT setwd() TO ENSURE APP RUNS

# getwd()
# setwd("C:/Users/peter/OneDrive/Documents/Athlete Lab")


# Load packages

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
library(lubridate)
library(paletteer)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggpubr)


# Import data

jun2824batter = fread("62824batter.csv")
jun2824pitcher = fread("62824pitcher.csv")

# Remove duplicate columns

jun2824batter <- subset(jun2824batter, select = -c(pitcher, fielder_2))
jun2824pitcher <- subset(jun2824pitcher, select = -c(pitcher, fielder_2))


# Create batter team column

jun2824batter$teamcol <- ifelse(jun2824batter$inning_topbot == "Top", jun2824batter$away_team,
                                jun2824batter$home_team)

# Create pitcher team column

jun2824pitcher$teamcol <- ifelse(jun2824pitcher$inning_topbot == "Top", jun2824pitcher$home_team,
                                jun2824pitcher$away_team)


# Cleaning up pitch names

# Removing blank pitch

jun2824batter <- jun2824batter[jun2824batter$pitch_name != ""]
jun2824pitcher <- jun2824pitcher[jun2824pitcher$pitch_name != ""]

# Adding knuckle curve & slurve to curveball

jun2824batter$pitch_name[jun2824batter$pitch_name == "Knuckle Curve"] <- "Curveball"
jun2824batter$pitch_name[jun2824batter$pitch_name == "Slurve"] <- "Curveball"
jun2824pitcher$pitch_name[jun2824pitcher$pitch_name == "Knuckle Curve"] <- "Curveball"
jun2824pitcher$pitch_name[jun2824pitcher$pitch_name == "Slurve"] <- "Curveball"

# Changing the name of 4-Seam Fastball & Split-Finger

jun2824batter$pitch_name[jun2824batter$pitch_name == "4-Seam Fastball"] <- "Fastball"
jun2824batter$pitch_name[jun2824batter$pitch_name == "Split-Finger"] <- "Splitter"
jun2824pitcher$pitch_name[jun2824pitcher$pitch_name == "4-Seam Fastball"] <- "Fastball"
jun2824pitcher$pitch_name[jun2824pitcher$pitch_name == "Split-Finger"] <- "Splitter"


# Inverting plate_x coordinates to get pitcher's view vs. catcher's view

jun2824batter$plate_x <- jun2824batter$plate_x * -1
jun2824pitcher$plate_x <- jun2824pitcher$plate_x * -1

# Multiplying movement coordinates by 12 to get inches instead of feet

jun2824batter$pfx_x <- jun2824batter$pfx_x * 12
jun2824batter$pfx_z <- jun2824batter$pfx_z * 12
jun2824pitcher$pfx_x <- jun2824pitcher$pfx_x * 12
jun2824pitcher$pfx_z <- jun2824pitcher$pfx_z * 12

# Inverting pfx_x coordinates to get pitcher's view vs. catcher's view

jun2824batter$pfx_x <- jun2824batter$pfx_x * -1
jun2824pitcher$pfx_x <- jun2824pitcher$pfx_x * -1


# Adding two columns for converted hit data for spray chart

jun2824batter$hit_x <- 2.5 * (jun2824batter$hc_x - 125.42)
jun2824batter$hit_y <- 2.5 * (198.27 - jun2824batter$hc_y)
jun2824pitcher$hit_x <- 2.5 * (jun2824pitcher$hc_x - 125.42)
jun2824pitcher$hit_y <- 2.5 * (198.27 - jun2824pitcher$hc_y)


# App UI

ui <- navbarPage("Peter Simon App", theme = shinytheme("flatly"),
                 
                 tabPanel("Players",
                          
                          tabsetPanel(
                            
                            tabPanel("Batters",
                                     
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         
                                         selectInput("BatterTeam", label = "Select Team",
                                                     
                                                     choices = levels(as.factor(jun2824batter$teamcol))),
                                         
                                         selectInput("Batter", label = "Select Batter",
                                                     
                                                     choices = levels(as.factor(jun2824batter$player_name))),
                                         
                                         dateRangeInput("BatterDate", label = "Select Date Range",
                                                        start = min(jun2824batter$game_date),
                                                        end = max(jun2824batter$game_date),
                                                        min = min(jun2824batter$game_date),
                                                        max = max(jun2824batter$game_date),
                                                        format = "yyyy-mm-dd",
                                                        separator = "to"),
                                         
                                         radioButtons("Throws", label = "Pitcher Throws",
                                                      
                                                      choices = levels(as.factor(jun2824batter$p_throws))),
                                         
                                         checkboxGroupInput("BatterPitch", label = "Pitch Type",
                                                            
                                                            choices = levels(as.factor(jun2824batter$pitch_name))),
                                         
                                         width = 3),
                                       
                                       mainPanel(
                                         
                                         br(),
                                         
                                         fluidRow(box(
                                           
                                           plotlyOutput("Batter_StrikeZone"), width = 12)),
                                         
                                         br(),
                                         
                                         fluidRow(box(
                                           
                                           plotlyOutput("Batter_SprayChart"), width = 12)),
                                       )
                                       
                                     )),
                            
                            # tabPanel("Pitchers",
                            #          
                            #          sidebarLayout(
                            #            
                            #            sidebarPanel(
                            #              
                            #              selectInput("PitcherTeam", label = "Select Team",
                            #                          
                            #                          choices = levels(as.factor(jun2824pitcher$pitcherteam))),
                            #              
                            #              selectInput("Pitcher", label = "Select Pitcher",
                            #                          
                            #                          choices = levels(as.factor(jun2824pitcher$player_name))),
                            #              
                            #              dateRangeInput("PitcherDate", label = "Select Date Range",
                            #                             start = min(jun2824pitcher$game_date),
                            #                             end = max(jun2824pitcher$game_date),
                            #                             min = min(jun2824pitcher$game_date),
                            #                             max = max(jun2824pitcher$game_date),
                            #                             format = "yyyy-mm-dd",
                            #                             separator = "to"),
                            #              
                            #              checkboxGroupInput("Stands", label = "Batter Stands",
                            #                                 
                            #                                 choices = levels(as.factor(jun2824pitcher$stand))),
                            #              
                            #              checkboxGroupInput("PitcherPitch", label = "Pitch Type",
                            #                                 
                            #                                 choices = levels(as.factor(
                            #                                   
                            #                                   jun2824pitcher$pitch_name))),
                            #              
                            #              width = 3),
                            #            
                            #            mainPanel(
                            #              
                            #              br(),
                            #              
                            #              fluidRow(box(
                            #                
                            #                plotlyOutput("Pitcher_StrikeZone"), width = 12)),
                            #              
                            #              br(),
                            #              
                            #              fluidRow(box(
                            #                
                            #                plotlyOutput("Pitcher_SprayChart"), width = 12)),
                            #              
                            #            ))),
                            
                          )))


# App Server


server = function(input, output, session) {
  
  # Reactives
  
  # Batter tab
  
  observeEvent(
    
    input$BatterTeam,
    
    updateSelectInput(session,
                      
                      "Batter", "Select Batter",
                      
                      choices = levels(factor(filter(jun2824batter,
                                                     
                                                     teamcol == isolate(input$BatterTeam))$player_name))))
  
  observeEvent(
    
    input$Batter,
    
    updateDateRangeInput(session,
                         
                         "BatterDate", "Select Date Range",
                         
                         start = min(jun2824batter$game_date),
                         
                         end = max(jun2824batter$game_date)))
  
  observeEvent(
    
    input$Batter,
    
    updateRadioButtons(session,
                       
                       "Throws", "Pitcher Throws",
                       
                       choices = levels(factor(filter(jun2824batter,
                                                      
                                                      player_name == isolate(input$Batter))$p_throws))))
  
  observeEvent(
    
    input$Batter,
    
    updateCheckboxGroupInput(session,
                             
                             "BatterPitch", "Pitch Type",
                             
                             choices = levels(factor(filter(jun2824batter,
                                                            
                                                            player_name == isolate(input$Batter))$pitch_name))))
  
  # Pitcher tab
  
  # observeEvent(
  #   
  #   input$PitcherTeam,
  #   
  #   updateSelectInput(session,
  #                     
  #                     "Pitcher", "Select Pitcher",
  #                     
  #                     choices = levels(factor(filter(jun2824pitcher,
  #                                                    
  #                                                    pitcherteam == isolate(input$PitcherTeam))$player_name))))
  # 
  # observeEvent(
  #   
  #   input$Pitcher,
  #   
  #   updateDateRangeInput(session,
  #                        
  #                        "PitcherDate", "Select Date Range",
  #                        
  #                        start = min(jun2824pitcher$game_date),
  #                        
  #                        end = max(jun2824pitcher$game_date)))
  # 
  # observeEvent(
  #   
  #   input$Stands,
  #   
  #   updateCheckboxGroupInput(session,
  #                            
  #                            "Stands", "Batter Stands",
  #                            
  #                            choices = levels(factor(filter(jun2824pitcher,
  #                                                           
  #                                                           player_name == isolate(input$Pitcher))$stand))))
  # 
  # observeEvent(
  #   
  #   input$Pitcher,
  #   
  #   updateCheckboxGroupInput(session,
  #                            
  #                            "PitcherPitch", "Pitch Type",
  #                            
  #                            choices = levels(factor(filter(jun2824pitcher,
  #                                                           
  #                                                           player_name == isolate(input$Pitcher))$pitch_name))))
  
  
  # Outputs
  
  # Batter tab
  
  # Strike Zone
  
  output$Batter_StrikeZone <- renderPlotly({
    
    Left <- -8.5 / 12
    Right <- 8.5 / 12
    Bottom <- 18.29 / 12
    Top <- 44.08 / 12
    
    Width <- (Right - Left) / 3
    Height <- (Top - Bottom) / 3
    
    jun2824batter %>%
      filter(player_name == input$Batter, pitch_name %in% input$BatterPitch, teamcol == input$BatterTeam,
             c(game_date >= input$BatterDate[1] & game_date <= input$BatterDate[2]), p_throws %in% input$Throws) %>%
      ggplot(jun2824batter, mapping = aes(x = plate_x, y = plate_z,
                                          label = release_speed,
                                          label2 = pfx_x,
                                          label3 = pfx_z,
                                          label4 = description,
                                          label5 = bb_type,
                                          label6 = events,
                                          label7 = hit_location)) +
      geom_point(aes(color = pitch_name), size = 3) +
      scale_color_manual(values = c(Fastball = "black", Curveball = "purple",
                                    Changeup = "blue", Sinker = "orange",
                                    Cutter = "red", Splitter = "pink",
                                    Slider = "yellow", Sweeper = "gold")) +
      
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
      
      xlim(-3,3) + ylim(0, 5)  + ggtitle("Pitch Location & Strikezone (Pitcher's View)") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
  })
  
  # Spray Chart
  
  output$Batter_SprayChart <- renderPlotly({
    
    jun2824batter %>%
      filter(player_name == input$Batter, teamcol == input$BatterTeam,
             c(game_date >= input$BatterDate[1] & game_date <= input$BatterDate[2])) %>%
      ggplot(jun2824batter, mapping = aes(x = hit_x, y = hit_y,
                                          label = pitch_name,
                                          label2 = bb_type,
                                          label3 = events,
                                          label4 = hit_distance_sc,
                                          label5 = launch_angle)) +
      geom_point(aes(fill = launch_speed), colour = "black", size = 3) +
      scale_fill_gradient2(low = "blue", high = "red",
                            midpoint = mean(86)) +
      xlim(-295, 295) + ylim(0, 450) +
      geom_segment(x = 0, xend = -315, y = 0, yend = 315, size = 1.2) +
      geom_segment(x = 0, xend = 315, y = 0, yend = 315, size = 1.2) +
      geom_curve(x = -315, xend = 315, y = 315, yend = 315, curvature = -.35, size = 1.2) +
      geom_curve(x = -90, xend = 90, y = 88, yend = 88, curvature = -.45, size = 1.2) +
      coord_fixed() + theme_bw() +
      
      # Format plot
      
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 16)) +
      labs(color = " ", title = "Spray Chart") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
  })
  
}


# Shiny App

shinyApp(ui = ui, server = server)
