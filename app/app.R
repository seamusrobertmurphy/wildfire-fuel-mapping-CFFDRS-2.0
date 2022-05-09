
#Action: Built as temporal dynamic user-controlled data platform fitted with BCMFLNRO Fuel-Typing Algorithm and CFFDRS Wildfire daily metrics   

#load packages
library(shiny)
library(leaflet)
library(sf)

#library(dygraphs) 
# Dygraphs fpr post-beta design to handle everything as xtc or zoo object 
# Seems the perfect solution for examining wildfire as a time-series framework

# import master dataframe (preprocessed - pre-user that is)
master_sf_interp = read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/wildfire/cffdrs_layers_bc4.shp")

# Design User-Interface here
ui <- fluidPage(
    # Application title
    titlePanel("Dynamic Fuel-Typing and Wildfire Metrics Platform"),
    # Sidebar items
        sidebarLayout(
          sidebarPanel(
            tags$a(href="https://docs.ropensci.org/nasapower/", "Climate API", target="_blank"),
            h5("Near real-time and historical data of forest fuel and wildfire weather conditions 
            are accessed below. Gridded climate data is provided through NASA's LANCE and 
            FIRMS satellite API's providing the most up-to-date access to MODIS and VIRS records 
            mapped within 3hours of sensing time. Ground truthing and remote scraping of canopy 
            and forest structure, along with sector updates from the upcoming wildfire season 
            are planned to develop the CabinFireApp as a source of public information and 
            improved mapping of current forest fuels and daily fire weather indices."),
          selectInput("date",
                      "Select date:",
                      choices = unique(master_sf_interp$date)
          )
        ),
        # Map portal
        mainPanel(
          tabsetPanel(
            tabPanel("1.1. Fuel Type", leafletOutput("fueltype")),
            tabPanel("1.2. Danger Rating", leafletOutput("dngr_rt")),
          )
        )
    )
)
            

# Define server logic here 
server <- function(input, output) {
  
  date_filter = reactive({
    day = master_sf_interp %>% filter(date == input$date)
    return(day)
  })
  
  output$fueltype = renderLeaflet({
    pal = colorBin(palette = "YlGn", 11, domain = master_sf_interp$fueltype)
    
    date_filter() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-119.25, 49.25, zoom=10) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(date_filter()$fueltype),
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
  })
  
  
  output$dngr_rt = renderLeaflet({
    pal = colorBin(palette = "RdYlBu", 5, domain = master_sf_interp$dngr_rt)
    
    date_filter() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-119.25, 49.25, zoom=10) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(date_filter()$dngr_rt),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    fillOpacity = 1,
                    color = "black", 
                    opacity = 1, 
                    bringToFront = TRUE)) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = ~ dngr_rt,
                title = "Drought Code",
                opacity=0.7)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
