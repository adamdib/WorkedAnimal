library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(leaflet.esri)
library(sf)
library(dplyr)
library(ggplot2)

shinyApp(
  ui = navbarPage("Worked Animal Materials", theme = shinytheme("flatly"),
                  tabPanel("Across Greece",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Materials"),
                               br(),
                               p("Use the sliders to filter sites based on their materials. 
                                 The sliders select minimum values, so setting Bone to 500 shows 
                                 all sites with more than 500 bone objects."),
                               br(),
                               sliderInput("sbone", "Bone:",
                                           min = 0, max = 500,
                                           value = 0),
                               sliderInput("santler", "Antler:",
                                           min = 0, max = 50,
                                           value = 0),
                               sliderInput("sivory", "Ivory:",
                                           min = 0, max = 200,
                                           value = 0),
                               sliderInput("stooth", "Non-Ivory Tooth:",
                                           min = 0, max = 10,
                                           value = 0)
                             ),
                             mainPanel(
                               h2("Across Greece"),
                               leafletOutput("selectedmap_var"),
                               p("Circular areas with numbers refer to a cluster of sites, 
                                 zoom in to see more. The size of the red circles is dictated 
                                 by the total number of worked animal objects at the site.")
                             )
                           )
                  ),
                  tabPanel("Case Study: Ancient Methone",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Materials"),
                               br(),
                               selectInput("Mat", "Material:",
                                           c("Bone" = "Bone",
                                             "Antler" = "Antler",
                                             "Ivory" = "Ivory",
                                             "Horncore" = "Horncore",
                                             "Unknown" = "Unknown",
                                             "Tooth" = "Tooth"), NULL, multiple = TRUE),
                               
                               selectInput("Area", "Area:",
                                           c("Area A" = "Area A",
                                             "Area B" = "Area B",
                                             "Area C" = "Area C"), NULL, multiple = TRUE),
                               br(),
                               p("Select at least one material and area.")
                             ),
                             mainPanel(
                               h2("Worked Animal Material Objects Across Excavation Areas"),
                               plotOutput("selected_var")
                             )
                           )
                           
                           
                  )
  ),
  server = function(input, output) { 
    
    output$selectedmap_var <-renderLeaflet({
      points = sf::st_read('data/XYNewMats.shp')
      points<- filter(points, BoneVal >= input$sbone & AntVal >= input$santler & IvoryVal >= input$sivory & ToothVal >= input$stooth)
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(lng = 25.000000, lat = 38.000000, zoom = 6) %>% 
        addCircleMarkers(data = points, fillColor = 'red', fillOpacity = 0.6, stroke = FALSE,
                         radius = sqrt(points$Total_Val), clusterOptions = markerClusterOptions(), label = points$Site)  }) 
    
    output$selected_var <-renderPlot({
      matdata <- read.csv("data/MaterialCount.csv")
      MaterialPlot<-ggplot() + geom_bar(aes(y = Count, x = Material, fill = ExcArea), data = matdata,
                                        stat="identity")
      newmatdata<- filter(matdata, ExcArea %in% input$Area & Material %in% input$Mat)
      p <- MaterialPlot<-ggplot() + geom_bar(aes(y = Count, x = ExcArea, fill = Material), data = newmatdata, stat="identity")+labs(x = "Excavation Areas") + theme(text=element_text(size=18))
      
      print(p)
    })
    
  }
)