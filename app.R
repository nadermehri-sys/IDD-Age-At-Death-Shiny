#setwd("~/Desktop/ShinyASI/APP")
#detect OS & set wd

os <- Sys.info()[1]

if(os != "Linux"){setwd ("C:/Users/mehrin/OneDrive - University of North Carolina at Chapel Hill/Desktop/CV_Cover_04-10/TL Revolution")}


library("htmltools")
library("shiny")
library("readxl")
library("tmap")
#library("maptools")
library(mapproj)
library("sf")
library("sp")
library(tidyverse)
library("tmaptools")
#library("oldtmaptools") #devtools::install_github("mtennekes/oldtmaptools")
#library("rgdal")
library("dplyr")
library("plyr")
library(shinyjs)
library(leaflet)
library(RColorBrewer)
library(plotly)
# require("sos")
# findFn("spTransform")
#library("profvis")
#profvis ({
########################
load("data.RData")
###############################################
blue.bold.14.text <-
  element_text(face = "bold",
               color = "black",
               size = 14)
blue.bold.12.text <-
  element_text(face = "bold",
               color = "black",
               size = 12)
blue.bold.10.text <-
  element_text(face = "bold",
               color = "black",
               size = 10)
blue.bold.8.text <-
  element_text(face = "bold",
               color = "black",
               size = 8)


#######################
#######################
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
  ####################################
#   #color for background and pages
#   #That's because the CSS class slider-animate-button has the property opacity: 0.5 defined in shiny.css. Set the opacity property to 1 with this CSS (to include in your UI):
#   tags$head(tags$style(
#     HTML(".slider-animate-button {opacity: 1;}")
#   )),
#   tags$head(tags$style(
#     HTML("hr {border-top: 2px solid #D44500;}")
#   )),
#   tags$style(
#     "button.navbar-toggle.collapsed {
#     background-image: url(collaped-menu-navbar-text.png);
#     background-size: contain;
#     background-repeat: no-repeat;
#     width: 70px;
#     }
#     .navbar-default .navbar-toggle .icon-bar {
#     background-color: transparent !important;
#     }"
# ),
# tags$style("
#            .collapse  {background-color: #D44500; color:white}
#            "),
# # background of sidebar panel to be gray and elemensts to be black
# tags$style(
#   "
#   .collapse.in  {background-color: transparent !important; color:black}
#   "
# ),
#
# tags$style("
#            .navbar-header  {background-color: #D44500; color:white}
#            "),
# # tags$style(
# #   HTML(
# #     "
# #    # .nav > li > a[data-value='Intellectual Disability'] {background-color: #D44500; color:white; font-size:16px}
# #     # .nav > li > a[data-value='Older Population'] {background-color: #D44500;  color:white}
# #     # .nav > li > a[data-value='Interactive Map'] {background-color: #D44500; color:white}
# #     # .nav > li > a[data-value='Grandparents'] {background-color: #D44500; color:white}
# #     # .nav > li > a[data-value='Functional Limitations'] {background-color: #D44500; color:white}
# #     # .nav > li > a[data-value='Social Indicators'] {background-color: #D44500; color:white}
# #     # .nav > li > a[data-value='Employment'] {background-color: #D44500; color:white}
# #     .nav > li[class=active]    > a {background-color: white !important; color:black}
# #     "
# #   )
# #   ),
# # tags$style(
# #   HTML(
# #     "
# #     .nav > li > a[data-value='Plot'] {background-color: #D44500; color:white}
# #     .nav > li > a[data-value='Pyramid'] {background-color: #D44500;  color:white}
# #     .nav > li > a[data-value='Data'] {background-color: #D44500; color:white}
# #     .nav > li > a[data-value='Interactive'] {background-color: #D44500;  color:white}
# #     .nav > li > a[data-value='Static Maps'] {background-color: #D44500; color:white}
# #     .nav > li > a[data-value='Grandparents'] {background-color: #D44500; color:white}
# #     .nav > li > a[data-value='Map Data'] {background-color: #D44500; color:white}
# #
# #     .nav > li > a[data-value='References'] {background-color: #D44500; color:white}
# #     .nav > li[class=active]    > a {background-color: #D44500; color:black}
# #     "
# #   )
# #   ),


tags$style(".btn{background-color:#D44500; color: white}"), #HTML ("<style>.btn{background-color:#D44500; color: white}</style>"),


#  tags$style(
#    ".col-sm-12{ background-color: #D44500; color:#B61E2E; border-style:solid}"
#  ),
# 
# tags$style(
#   ".col-sm-10{background-color: #D44500; color:#B61E2E; border-style:solid}"
# ),

# tags$style(
 #  ".shiny-text-output {background-color: #D44500; color:white;  border-style:solid; border: 1px solid #000;}"
# ),
# tags$style(
#   ".shiny-table {background-color: ; color: no-color; border: 1px solid;}"
# ),
# tags$style(
#   ".head{background-color: #D44500; color:white; border: 1px solid #000;}"
# ),
# tags$style(
#   ".h1{background-color: #D44500; color:white; border: 1px solid #000;}"
# ),
# tags$script(
#   HTML(
#     "
#     $(document).on('click','.navbar-collapse.in',function(e) {
#     if( $(e.target).is('a') ) {
#     $(this).collapse('hide');
#     }
#     });
#     "
#   )
# ),
###################################
# navbarPage(  # if you need the navbar page, you enable a para at line #634
#   collapsible = T,
#   fluid = T,
#   windowTitle = "Aging Studies Interactive Data Center",
#   "",
#   selected = "Intelectual Disability",

   # tabPanel(
   #   "Intelectual Disability",

    tags$h4 (
      strong(
        "Intellectual and Developmental Disability (IDD) Age at Death Data Tracker (2008-2017)"
      )
    ),
    br(),
    #a line break
    fluidRow(#width=12,
    sidebarLayout(
      column(width=4,
      sidebarPanel(width=12,
                   
                   #loading massage
                   
        radioButtons(
          inputId = "Disability_map",
          label = strong("Type of Disability"),
          selected = "No Intellectual and Developmental Disability",
          choices = sort(unique(IDD_nhmap$Disability))
        ),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...Please Wait",id="loadmessage")),
        #this cloeses radioButtons
         br(),
         br(),
         # ### Download Button
         # downloadButton("downloadData_map", "Download Data (Selected Disability)"),
         # br(),
         # br(),
         downloadButton("IDD_download_data_map", "Download Map Data"),
         br(),
         br(),
         downloadButton("IDD_download_age_map", "Download Age Data"),
         br(),
         br(),
         downloadButton("IDD_download_sex_map", "Download Sex Data"),
         br(),
         br(),
         downloadButton("IDD_download_race_map", "Download Race Data")
        

      )),
      # this closes sidebarPanel
      ######################
      mainPanel(
        # tabsetPanel(type = "tabs",
        # tabPanel("Interactive Map", leafletOutput("IDD_int_map", height=400),
        fluidRow(
          
          #loading massage#3: add: shinyjs::useShinyjs() 
          #
          column(width = 12, shinyjs::useShinyjs(), leafletOutput("IDD_int_map", height = "445px")))
        
        #loading massage#2
        
        
        ) # this closes mainPanel
      ) # this closes sidebarLayout
    ), # this closes fluidRow
        br(),
        br(),
        ######################
      #  plotlyOutput("IDD_plot_year", height = 400,
    fluidRow(column(width = 12,  plotlyOutput("IDD_plot_year",  height = "450px"))),
    br(),
    
    fluidRow(column(width = 10, plotlyOutput("IDD_plot_sex", height = "600px"))),
    
    fluidRow(column(width = 12, plotlyOutput("IDD_plot_race",  height = "600px"))),

    
        #############################
        #defining the data source, info, ...
        #Contact
        tags$div(
          id = "cite",
          'Contact',
          style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
          white-space: nowrap; overflow: visible"
        ),
        div(tags$div(
          "",
          a("Scott Landes, PhD",
            href = "https://www.maxwell.syr.edu/soc/Landes,_Scott/", target = "_blank"),
          style = "text-align: left; font-size:14px"
        )),
        br(),
        #development
        #Contact
        tags$div(
          id = "cite",
          'Developed by',
          style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
          white-space: nowrap; overflow: visible"
        ),
        div(tags$div(
          "",
          a("Nader Mehri, PhD",
            href = "https://scholar.google.com/citations?user=2uEQM5sAAAAJ&hl=en", target = "_blank"),
          style = "text-align: left; font-size:14px"
        )),
        br(),
        tags$div(
          id = "cite",
          'Data Source(s)',
          style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
          white-space: nowrap; overflow: visible"
        ),
        div(tags$div(
          "",
          a(
            "2008–2017 U.S. Multiple Cause-of-Death Mortality Files, National Vital Statistics System",
            href = "https://www.cdc.gov/nchs/nvss/index.htm",
            target = "_blank"
          ),
          style = "text-align: left; font-size:14px"
        )),
        br(),
        #Data information
        tags$div(
          id = "cite",
          'Data Information:',
          style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
          white-space: nowrap; overflow: visible"
        ),
        tags$div(
          id = "cite",
          'Trends are for adults, age 18 and over, who died between January 1, 2008 and December 31, 2017. Disability categories are based upon ICD-10 diagnoses codes: Intellectual disability (F70-79); cerebral palsy (G80.0-80.9); Down syndrome (Q90.0-90.9); Other rare developmental disabilities (Q91.0-93.9; F84.0-84.9; Q86.0)',
          style = "color: black; font-size: 14px ; width: 100%;
          white-space: pre-line"
        ),
        br(),
        #Funding information
        tags$div(
          id = "cite",
          'Funding Information:',
          style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
          white-space: nowrap; overflow: visible"
        ),
        div(
          tags$div(
            "Initial funding for this project was provided through a",
            a("Tenth Decade Project",
              href = "https://www.maxwell.syr.edu/TenthDecadeProject/",
              target = "_blank"),
            "Grant,",
            a(
              "Maxwell School of Citizenship and Public Affairs",
              href = "https://www.maxwell.syr.edu/",
              target = "_blank"
            ),
            ", Syracuse University."
          ),
          style = "text-align: left; font-size:14px"
        ),
        br(),
        #Term of Use
        #Data information
        tags$div(
          id = "cite",
          'Terms of Use:',
          style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
          white-space: nowrap; overflow: visible"
        ),
        tags$div(
          id = "cite",
          '© 2020 Syracuse University. All rights reserved. This website is based upon publicly available data from the National Vital Statistics System (NVSS) and is provided "as-is", without any representations or warranties of any kind, including without limitation with respect to accuracy and completeness, availability, suitability for a particular purpose and non-infringement. It is provided by Syracuse University for educational and research purposes only. We invite you to view and link to this website, but it may not be copied, reproduced, framed, downloaded, redistributed or displayed (in whole or in part) without the University consent. Any use of the University name, logo, seal or other trademarks also requires the University consent.',
          style = "color: black; font-size: 14px ; width: 100%;
          white-space: pre-line"
        ),
    br(),
    
    #Related Publications
    tags$div(
      id = "cite",
      'Related Publications:',
      style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
      white-space: nowrap; overflow: visible"
    ),
    div(
      tags$div(
        "Landes, Scott D., J. Dalton Stevens and Margaret A. Turk. 2019. Heterogeneity in Age at Death for Adults with Developmental Disability. Journal of Intellectual Disability Research, 63:1482-87. doi: 10.1111/jir.12672. Link:",
        a("https://onlinelibrary.wiley.com/doi/full/10.1111/jir.12672",
          href = "https://onlinelibrary.wiley.com/doi/full/10.1111/jir.12672",
          target = "_blank")
      ),
      style = "text-align: left; font-size:14px"
    ),
    
    br(),
    
    div(
      tags$div(
        "Landes, Scott D., Katherine E. McDonald, Janet M. Wilmoth and Erika Carter Grosso. 2020. Evidence of Continued Reduction in the Age-at-Death Disparity between Adults with and without Intellectual and/or Developmental Disabilities. Journal of Applied Research in Intellectual Disabilities. doi: 10.1111/jar.12840. Link:",
        a("https://onlinelibrary.wiley.com/doi/10.1111/jar.12840",
          href = "https://onlinelibrary.wiley.com/doi/10.1111/jar.12840",
          target = "_blank"),
      style = "text-align: left; font-size:14px")),
    
    br(),
    
    #Last Modified
    tags$div(
      id = "cite",
      'Last Modified: 04/17/2021',
      style = "color: black; font-size: 14px ; width: 220px;font-weight:bold;
      white-space: nowrap; overflow: visible"
    )
    
        #FAQ
        # div(tags$strong(
        #   "*See the",
        #   a("FAQ", href = "http://miamioh.edu/cas/academics/centers/scripps/research/ohio-population/faq/index.html", target = "_blank"),
        #   "for more information."
        # ),
        # style = "text-align: left; font-size:14px")
        ############################
       # ) # this closes tab  tabPanel("Interactive")
      #   ) # this closes tabset panel
    #    ) #this closes main panel
   # ) # thise closes fluidRow
     #   ) #this closes sidebar layout
#) # this closes column
  # ) # this closes tabPanel
  ########################
 # ) #this closes navbarPage
) # this closes ui
######################
#######################
#pick up your color of intrest
#Hexadecimal color specification
# display.brewer.all()
# brewer.pal(n = 8, name = "YlOrRd")
# display.brewer.pal(n = 8, name = 'YlOrRd')
####################################
server <- function(input, output, session) {
  
  
  #sort the cat values given the disability values: you need this to get a sorted legend
  IDD_nhmap <- IDD_nhmap %>%
    arrange(desc(Disability == "Intellectual disability"), desc(cat)) %>%
    mutate(cat = factor(cat, levels = unique(cat)))
  # if you want to do this for more levels of disability then:
  # IDD_nhmap <- IDD_nhmap %>%
  #   arrange(factor (Disability, levels= c('Intellectual disability','Intellectual disability')), cat) %>%
  #   mutate(cat = factor(cat, levels = unique(cat)))
  IDD_mapdata_ <- reactive ({
    IDD_nhmap$AgeatDeath <- round(IDD_nhmap$AgeatDeath, 1)
    out_map <- IDD_nhmap %>%
      filter (Disability %in% input$Disability_map)
    return(out_map)
    list(AgeatDeath)
  })
  IDD_mapdata_1 <- reactive ({
    IDD_nhmap$AgeatDeath <- round(IDD_nhmap$AgeatDeath, 1)
    out_map_1 <- IDD_nhmap %>%
      filter (Disability %in% input$Disability_map)
    return(out_map_1)
    list(cat)
  })
  output$IDD_int_map <- renderLeaflet ({
    
    #loading message: add below 2 lines to disable loading message when select a choice
   
   
  
    # pal8 <- c("#FFFFE5", "#D9F0A3", "#78C679", "#006837")
    # pal <- colorBin(palette = pal8, domain =NULL, bins=quantile(IDD_mapdata_1()$Age), na.color = "#808080",  alpha = FALSE, reverse = F)
    pal <-
      colorFactor(
        palette = c("#FF9000", "#FFBA00", "#5062FB", "mediumblue") ,
        
         domain = NULL,
        levels = (IDD_mapdata_1()$cat),
        ordered = TRUE,
        na.color = "#808080",
        alpha = FALSE,
        reverse = F
      )
    labels <- sprintf(
      "<strong>%s </strong> <br/> Age at Death = %s <br/> Rank = %s out of 50 States and DC",
      IDD_mapdata_()$NAME,
      IDD_mapdata_()$AgeatDeath,
      IDD_mapdata_()$rank
    ) %>%
      lapply(htmltools::HTML)
  
    #Sys.sleep(1)
    
  leaflet (IDD_mapdata_(), options = leafletOptions(zoomSnap = 0.25, zoomDelta =
                                                        0.25)) %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(opacity = 0)) %>%  # you need this and ()to remove the backgroun (Mexico/Canda)
      clearControls() %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ pal(cat),
        stroke = T,
        weight = 1,
        smoothFactor = 0.2,
        fillOpacity = 1,
        color = "black",
        # label=~paste0(NAME," ","County",":"," ",input$sex_map,",", " ",
        #              input$Disability_map,"=",Age,"%"),
        label = labels,
        labelOptions = labelOptions(
          interactive = TRUE,
          style = list(
            'direction' = 'auto',
            'color' =
              'black',
            'font-family' = 'sans-serif',
            # 'font-style'= 'italic',
            'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
            'font-size' = '14px',
            'border-color' = 'rgba(0,0,0,0.5)'
          )
        ),
        # label=~paste(NAME,"<br>",input$sex_map,
        #              input$Disability_map,"=",Age,"%"),
        
        # label = lapply(labs, htmltools::HTML),
        highlightOptions = highlightOptions(
          #color = "red",
          weight = 2,
          bringToFront = T,
          # color = "#666",
          fillOpacity = 0.7
        )
      ) %>%
    setView(lng = -95.0167,
            lat = 37.8833,
            zoom = 3.5) %>%
      addLegend(
        position = "topright",
        opacity = 1.0,
        values = ~ cat,
        #  colors= c("#FFFFE5", "#D9F0A3", "#78C679", "#006837"),
        pal = pal,
        # title = (paste("%",input$Disability_map)) ,
        title = (paste("Age at Death Quartiles")) ,
        labFormat = labelFormat()
      ) %>%
      addTiles(options = tileOptions(opacity = 0))  # you need this to remove the backgroun (Mexico/Canda)
 
     })
  ######################
  #download the map data
  IDD_download <-  subset(IDD_map, select = -c(cat))
  
  output$IDD_download_data_map <- downloadHandler(
    filename = function() {
      paste("IDD_Map_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_download, file, row.names = T)
    }
  )
  ######################
  #download the age data
 # IDD_download_age <-  subset(IDD_map, select = -c(cat))
  
  output$IDD_download_age_map <- downloadHandler(
    filename = function() {
      paste("IDD_Age_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_plot_year, file, row.names = T)
    }
  )
  #################################################
  #download the sex data
  # IDD_download_age <-  subset(IDD_map, select = -c(cat))
  
  output$IDD_download_sex_map <- downloadHandler(
    filename = function() {
      paste("IDD_sex_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_plot_sex, file, row.names = T)
    }
  )
  #################################################
  #download the race data
  # IDD_download_age <-  subset(IDD_map, select = -c(cat))
  
  output$IDD_download_race_map <- downloadHandler(
    filename = function() {
      paste("IDD_race_All", Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv(IDD_plot_race, file, row.names = T)
    }
  )
  #################################################
 # "<b style='color: mediumblue'> Year: %g <br>Age at Death: %g </b>",
 # "<b> Year: %g </b> <br>Age at Death: %g"
 
  
  IDD_line_year <-
    ggplot(IDD_plot_year) +
    aes(color = Disability, 
        shape = Disability, 
        group = Disability, 
                text = sprintf("<b style='color: black'> Year: %g <br>Age at Death: %g </b>", 
                       Year, AgeatDeath))+ 
   
     scale_colour_manual(values=c("chartreuse1", "chocolate1", "mediumvioletred", "darkorchid1", "gold1")) + 
    
    geom_line(aes(x = Year, y = AgeatDeath), size = 1) +
    geom_point(aes(x = Year,
                   y = AgeatDeath
                   ),
               size = 2
               ) +
    scale_x_continuous(breaks = seq(2008, 2017, 1)) +
    labs(
      x = "",
      y = "Age at Death",
      caption = (""),
      face = "bold"
    ) +
    theme_bw() +
    scale_y_continuous(breaks = seq(35, 80, 5)) +
    ggtitle(paste("<b>Age of Death by Type of Disability and Year (2008 to 2017)</b>")) +
    theme(
      legend.title = element_blank(),
      legend.key=element_blank(),
      legend.text = element_text(size = 10),
      legend.background=element_blank(),
      axis.text = (blue.bold.10.text),
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = -45, vjust = 0, hjust=0)
    )
  output$IDD_plot_year <- renderPlotly(
    ggplotly(IDD_line_year, tooltip = "text") %>%
      config(displayModeBar = F) %>%
      
    layout(#legend = list(xanchor = "center",x = 0.5, y = 1.2, orientation = "h"),
      #margin = list(l = 50, r = 50, b = 100, t = 100),
      title = list(xanchor = "center", x = .5))
  )
  ######################################
  #sex
  IDD_line_sex <-
    ggplot(IDD_plot_sex) +
    aes(
        text = sprintf("<b style='color: black'> Sex: %s <br>Age at Death: %g </b>",
          
         # "Sex: %s<br>Age at Death: %g", 
                       sex, AgeatDeath)) +
    
    scale_fill_manual(values=c("#1A85FF", "#D41159")) + 
    
    geom_bar(aes(x = Disability, y = AgeatDeath, fill=sex), colour="black",
             stat="identity", width=0.9, position = position_dodge())+
        labs(
      x = "",
      y = "Age at Death",
      caption = (""),
      face = "bold"
    ) +
    theme_bw() +
   # scale_y_continuous(breaks = seq(35, 80, 5)) +
    ggtitle(paste("<b>Age of Death by Type of Disability and Sex (2008 to 2017)</b>")) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.text = (blue.bold.10.text),
      axis.text.x = element_text(angle = -45, vjust = 0, hjust=0),
      plot.title = element_text(size = 12, face = "bold")
    )
  output$IDD_plot_sex <- renderPlotly(
    ggplotly(IDD_line_sex, tooltip = "text") %>%
      config(displayModeBar = F) %>%
      layout( #legend = list(xanchor = "center",x = 0.5, y = 1.15, orientation = "h"),
            # margin = list(l = 50, r = 50, b = 100, t = 100),
             title = list(xanchor = "center", x = .5))
  )
  ##########################################
  ######################################
  #race
  
  output$IDD_plot_race <- renderPlotly({
  
    
      Sys.sleep(1)
    
    
             IDD_line_race <-
               ggplot(IDD_plot_race) +
               aes(
                 text = sprintf("<b style='color: black'> Race: %s <br>Age at Death: %g </b>",
                                
                                race, AgeatDeath)) +
               
               geom_bar(aes(x = Disability, y = AgeatDeath, fill=race), colour="black",
                        stat="identity", width=0.9,  position = position_dodge()) +
               
               
               # scale_fill_manual(values=c("chartreuse3", "slateblue1", "firebrick1", "gold", "peru", "magenta2")) + 
               
               scale_fill_manual(values=c("#F0E442", "#56B4E9", "#009E73","#E69F00", "#0072B2", "#D55E00")) + 
               
               labs(
                 x = "",
                 y = "Age at Death",
                 caption = (""),
                 face = "bold"
               ) +
               theme_bw() +
               # scale_y_continuous(breaks = seq(35, 80, 5)) +
               ggtitle(paste("<b>Age of Death by Type of Disability and Race (2008 to 2017)</b>")) +
               theme(
                 legend.title = element_blank(),
                 axis.text = (blue.bold.10.text),
                 legend.text = element_text(size = 10),
                 legend.key=element_blank(),
                 legend.background=element_blank(),
                 axis.text.x = element_text(angle = -45, vjust = 0, hjust=0),
                 plot.title = element_text(size = 12, face = "bold")
               ) 
             

  ggplotly(IDD_line_race, tooltip = "text") %>%
             
             config(displayModeBar = F) %>%
             layout(
               
               # legend = list(xanchor = "center",x = 0.5, y = 1.2, orientation = "h"),
               # margin = list(l = 50, r = 50, b = 100, t = 100),
               # title = paste("<b>Age of Death by Type of Disability and Race (2008 to 2017)</b>"), list(xanchor = "center", x = .5)) 
               title= list(xanchor = "center", x = .5))
  shinyjs::hide("loadmessage")
  ggplotly(IDD_line_race, tooltip = "text") %>%
    
    config(displayModeBar = F) %>%
    layout(
      
      # legend = list(xanchor = "center",x = 0.5, y = 1.2, orientation = "h"),
      # margin = list(l = 50, r = 50, b = 100, t = 100),
      # title = paste("<b>Age of Death by Type of Disability and Race (2008 to 2017)</b>"), list(xanchor = "center", x = .5)) 
      title= list(xanchor = "center", x = .5))
  })
  ##########################################
  
}
shinyApp(ui, server)
