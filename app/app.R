
#Action: Built as temporal dynamic user-controlled data platform fitted with BCMFLNRO Fuel-Typing Algorithm and CFFDRS Wildfire daily metrics   

#load packages
library(shiny)
library(leaflet)


#library(dygraphs) 
# Dygraphs fpr post-beta design to handle everything as xtc or zoo object 
# Seems the perfect solution for examining wildfire as a time-series framework

# import master dataframe (preprocessed - pre-user that is)
master_sf_interp = readRDS("./app/master_sf_interp.RDS")

# Design User-Interface here
ui <- fluidPage(
    # Application title
    titlePanel("Dynamic Fuel-Typing and Wildfire Metrics Platform"),
    # Sidebar items
        sidebarLayout(
          sidebarPanel(
            tags$a(href="https://docs.ropensci.org/nasapower/", "Climate API", target="_blank"),
            h5("Near real-time data streaming was powered by NASAPower's R-friendly API 
            and MFLNRO's Geographic Warehouse providers. This includes loop feeds of within
            three hours of sensing time from NASA's LANCE and FIRMS dashboards. Along with 
            ground truthing and sector updates, the platform maintains an up-to-date resource 
            of data on forest fuels for downloading, preparing, cross-platform sharing, or for 
            oprational planning, surveillance and front-line responders across British Coloumbia."),
          selectInput("date",
                      "Select date:",
                      choices = unique(master_sf_interp$date)
          )
        ),
        # Map portal
        mainPanel(
          tabsetPanel(
            tabPanel("Fuel Type", leaflet)
            
            
            put("date_selection")
      
      
      
      leafletOutput("map_interactive"))
    )

# Define server logic here 
server <- function(input, output) {
  
  #create date filter input
  output$seasons_select = renderUI({
    selectInput("pick_date", "Select date:", choices = c(unique(master_sf_interp$date))
  })
    
  #create map
  output$map_interactive = renderLeaflet([

    #setup map #?addProviderTiles
    map_interactive = leaflet(data=master_sf_interp) %>%
    map_interactive = addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(fueltype),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    fillOpacity = 1,
                    color = "black", 
                    opacity = 1, 
                    bringToFront = TRUE)) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = ~ fueltype,
                title = "Fuel Type",
                opacity=0.7)
    
  ])
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
