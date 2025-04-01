###############################################
# IDD Age-at-Death Data Tracker Shiny App
# Original by: Nader Mehri
# Updated: 2025-XX-XX
###############################################

# If needed, set working directory conditionally:
os <- Sys.info()[1]
if(os != "Linux") {
  setwd("Your Directory")
}

library("htmltools")
library("shiny")
library("readxl")
library("tmap")
library("mapproj")
library("sf")
library("sp")
library(tidyverse)
library("tmaptools")
library("dplyr")
library("plyr")
library(shinyjs)
library(leaflet)
library(RColorBrewer)
library(plotly)
###############################################
load("data.RData")
###############################################
# Some text styling for ggplot:
blue.bold.14.text <- element_text(face = "bold", color = "black", size = 14)
blue.bold.12.text <- element_text(face = "bold", color = "black", size = 12)
blue.bold.10.text <- element_text(face = "bold", color = "black", size = 10)
blue.bold.8.text  <- element_text(face = "bold", color = "black", size = 8)

###############################################
##########        UI SECTION       ############
###############################################
ui <- fluidPage(
  
  tags$head(tags$style(type="text/css", "
    #loadmessage {
      position: fixed;
      top: 0px;
      left: 0px;
      width: 100%;
      padding: 5px 0px 5px 0px;
      text-align: center;
      font-weight: bold;
      font-size: 150%;
      color: #000000;
      background-color: #D44500!important;
      z-index: 105;
    }
  ")),
  
  # Minimal custom button color
  tags$style(".btn{background-color:#D44500; color: white}"),
  
  shinyjs::useShinyjs(),
  
  tags$h4(strong("Intellectual and Developmental Disability (IDD) Age at Death Data Tracker (2008-2017)")),
  br(),
  
  fluidRow(
    sidebarLayout(
      column(width=4,
             sidebarPanel(width=12,
                          # Radio Buttons for Disability Type
                          radioButtons(
                            inputId = "Disability_map",
                            label = strong("Type of Disability"),
                            selected = "No Intellectual and Developmental Disability",
                            choices = sort(unique(IDD_nhmap$Disability))
                          ),
                          
                          # Loading message
                          conditionalPanel(
                            condition="$('html').hasClass('shiny-busy')",
                            tags$div("Loading...Please Wait", id="loadmessage")
                          ),
                          br(), br(),
                          
                          # Download Buttons
                          downloadButton("IDD_download_data_map", "Download Map Data"),
                          br(), br(),
                          downloadButton("IDD_download_age_map", "Download Age Data"),
                          br(), br(),
                          downloadButton("IDD_download_sex_map", "Download Sex Data"),
                          br(), br(),
                          downloadButton("IDD_download_race_map", "Download Race Data")
             )
      ),
      
      mainPanel(
        fluidRow(
          column(width = 12, leafletOutput("IDD_int_map", height = "445px"))
        )
      )
    )
  ),
  
  br(), br(),
  
  fluidRow(column(width = 12,  plotlyOutput("IDD_plot_year",  height = "450px"))),
  br(),
  fluidRow(column(width = 10, plotlyOutput("IDD_plot_sex", height = "600px"))),
  fluidRow(column(width = 12, plotlyOutput("IDD_plot_race",  height = "600px"))),
  
  br(),
  
  # Contact and references
  tags$div(id = "cite", 'Contact', style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;"),
  div(tags$div("", a("Scott Landes, PhD", href="https://www.maxwell.syr.edu/soc/Landes,_Scott/", target="_blank"),
               style = "text-align: left; font-size:14px")),
  br(),
  
  tags$div(id = "cite", 'Developed by', style = "color: black; font-size:14px; width:220px;font-weight:bold;"),
  div(tags$div("", a("Nader Mehri, PhD",
                     href="https://scholar.google.com/citations?user=2uEQM5sAAAAJ&hl=en", target="_blank"),
               style = "text-align: left; font-size:14px")),
  br(),
  
  # Data & Terms of Use info, disclaimers, references, etc.
  tags$div(id = "cite", 'Data Source(s)', style = "color: black; font-size:14px;font-weight:bold;"),
  # e.g. link to NVSS or other sources
  br(),
  tags$div(id = "cite", 'Last Modified: 04/17/2021',
           style = "color: black; font-size:14px;width:220px;font-weight:bold;")
)

###############################################
##########       SERVER SECTION     ###########
###############################################
server <- function(input, output, session) {
  
  # Reactive filter for Leaflet map
  IDD_mapdata_ <- reactive ({
    IDD_nhmap$AgeatDeath <- round(IDD_nhmap$AgeatDeath, 1)
    out_map <- IDD_nhmap %>%
      filter (Disability %in% input$Disability_map)  # Potential mismatch if data lacks "No IDD"
    return(out_map)
  })
  
  IDD_mapdata_1 <- reactive ({
    IDD_nhmap$AgeatDeath <- round(IDD_nhmap$AgeatDeath, 1)
    out_map_1 <- IDD_nhmap %>%
      filter (Disability %in% input$Disability_map)
    return(out_map_1)
  })
  
  # Main Leaflet map
  output$IDD_int_map <- renderLeaflet({
    pal <- colorFactor(
      palette = c("#FF9000", "#FFBA00", "#5062FB", "mediumblue"),
      domain = NULL,
      levels = (IDD_mapdata_1()$cat),
      ordered = TRUE,
      na.color = "#808080",
      alpha = FALSE,
      reverse = FALSE
    )
    
    labels <- sprintf(
      "<strong>%s </strong> <br/> Age at Death = %s <br/> Rank = %s out of 50 States and DC",
      IDD_mapdata_()$NAME,
      IDD_mapdata_()$AgeatDeath,
      IDD_mapdata_()$rank
    ) %>% lapply(htmltools::HTML)
    
    leaflet(IDD_mapdata_(), options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25)) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0)) %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ pal(cat),
        stroke = TRUE,
        weight = 1,
        smoothFactor = 0.2,
        fillOpacity = 1,
        color = "black",
        label = labels,
        labelOptions = labelOptions(
          interactive = TRUE,
          style = list(
            'direction' = 'auto',
            'color' = 'black',
            'font-family' = 'sans-serif',
            'font-size' = '14px'
          )
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          bringToFront = TRUE,
          fillOpacity = 0.7
        )
      ) %>%
      setView(lng = -95.0167, lat = 37.8833, zoom = 3.5) %>%
      addLegend(
        position = "topright",
        opacity = 1.0,
        values = ~ cat,
        pal = pal,
        title = (paste("Age at Death Quartiles")),
        labFormat = labelFormat()
      ) %>%
      addTiles(options = tileOptions(opacity = 0))
  })
  
  # Download Handlers - Map
  IDD_download <- subset(IDD_map, select = -c(cat))
  output$IDD_download_data_map <- downloadHandler(
    filename = function() {
      paste("IDD_Map_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_download, file, row.names = TRUE)
    }
  )
  
  # Age data
  output$IDD_download_age_map <- downloadHandler(
    filename = function() {
      paste("IDD_Age_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_plot_year, file, row.names = TRUE)
    }
  )
  
  # Sex data
  output$IDD_download_sex_map <- downloadHandler(
    filename = function() {
      paste("IDD_sex_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_plot_sex, file, row.names = TRUE)
    }
  )
  
  # Race data
  output$IDD_download_race_map <- downloadHandler(
    filename = function() {
      paste("IDD_race_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_plot_race, file, row.names = TRUE)
    }
  )
  
  # Plot for Year
  IDD_line_year <- ggplot(IDD_plot_year) +
    aes(color = Disability, shape = Disability, group = Disability,
        text = sprintf("<b style='color: black'> Year: %g <br>Age at Death: %g </b>", Year, AgeatDeath)) +
    # set color manually if desired
    geom_line(aes(x = Year, y = AgeatDeath), size = 1) +
    geom_point(aes(x = Year, y = AgeatDeath), size = 2) +
    scale_x_continuous(breaks = seq(2008, 2017, 1)) +
    labs(x = "", y = "Age at Death", caption = ("")) +
    theme_bw() +
    scale_y_continuous(breaks = seq(35, 80, 5)) +
    ggtitle(paste("<b>Age of Death by Type of Disability and Year (2008 to 2017)</b>")) +
    theme(
      legend.title = element_blank(),
      legend.key = element_blank(),
      axis.text = (blue.bold.10.text),
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = -45, vjust = 0, hjust=0)
    )
  
  output$IDD_plot_year <- renderPlotly(
    ggplotly(IDD_line_year, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(xanchor = "center", x = .5))
  )
  
  # Plot for Sex
  IDD_line_sex <- ggplot(IDD_plot_sex) +
    aes(text = sprintf("<b style='color: black'> Sex: %s <br>Age at Death: %g </b>", sex, AgeatDeath)) +
    geom_bar(aes(x = Disability, y = AgeatDeath, fill=sex),
             colour="black", stat="identity", width=0.9, position = position_dodge()) +
    labs(x = "", y = "Age at Death", caption = ("")) +
    theme_bw() +
    ggtitle(paste("<b>Age of Death by Type of Disability and Sex (2008 to 2017)</b>")) +
    theme(
      legend.title = element_blank(),
      axis.text = (blue.bold.10.text),
      axis.text.x = element_text(angle = -45, vjust = 0, hjust=0),
      plot.title = element_text(size = 12, face = "bold")
    )
  
  output$IDD_plot_sex <- renderPlotly(
    ggplotly(IDD_line_sex, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(xanchor = "center", x = .5))
  )
  
  # Plot for Race
  output$IDD_plot_race <- renderPlotly({
    IDD_line_race <- ggplot(IDD_plot_race) +
      aes(text = sprintf("<b style='color: black'> Race: %s <br>Age at Death: %g </b>", race, AgeatDeath)) +
      geom_bar(aes(x = Disability, y = AgeatDeath, fill=race),
               colour="black", stat="identity", width=0.9, position = position_dodge()) +
      scale_fill_manual(values=c("#F0E442", "#56B4E9", "#009E73","#E69F00", "#0072B2", "#D55E00")) +
      labs(x = "", y = "Age at Death", caption = ("")) +
      theme_bw() +
      ggtitle(paste("<b>Age of Death by Type of Disability and Race (2008 to 2017)</b>")) +
      theme(
        legend.title = element_blank(),
        axis.text = (blue.bold.10.text),
        legend.key=element_blank(),
        legend.background=element_blank(),
        axis.text.x = element_text(angle = -45, vjust = 0, hjust=0),
        plot.title = element_text(size = 12, face = "bold")
      )
    
    ggplotly(IDD_line_race, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(title= list(xanchor = "center", x = .5))
  })
}

shinyApp(ui, server)
