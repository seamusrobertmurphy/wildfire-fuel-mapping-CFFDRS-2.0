
#Action: Built as temporal dynamic user-controlled data platform fitted with BCMFLNRO Fuel-Typing Algorithm and CFFDRS Wildfire daily metrics   

#load packages
library(shiny)
library(leaflet)

#library(dygraphs) 
# Dygraphs fpr post-beta design to handle everything as xtc or zoo object 
# Seems the perfect solution for examining wildfire as a time-series framework

# import master dataframe (preprocessed - pre-user that is)
master_sf_interp = readRDS("./master_sf_interp.RDS")

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
            tabPanel("1.2. Drought Code", leafletOutput("DC")),
            tabPanel("2.1. Initial Spread Index", leafletOutput("ISI")),
            tabPanel("2.2. Fire Weather Index", leafletOutput("FWI")),
            tabPanel("3.1. Head Fire Intensity (kW/m)", leafletOutput("HFI")),
            tabPanel("3.2. Spread Direction Azimuth", leafletOutput("RAZ")),
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
  
  
  output$DC = renderLeaflet({
    pal = colorBin(palette = "RdYlBu", 5, domain = master_sf_interp$DC)
    
    date_filter() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-119.25, 49.25, zoom=10) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(date_filter()$DC),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    fillOpacity = 1,
                    color = "black", 
                    opacity = 1, 
                    bringToFront = TRUE)) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = ~ DC,
                title = "Drought Code",
                opacity=0.7)
    })
    
  
  output$ISI = renderLeaflet({
    pal = colorBin(palette = "R3", 5, domain = master_sf_interp$ISI)
    
    date_filter() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "NASAGIBS.ModisTerraTrueColorCR") %>%
      setView(-119.25, 49.25, zoom=10) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(date_filter()$ISI),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    fillOpacity = 1,
                    color = "black", 
                    opacity = 1, 
                    bringToFront = TRUE)) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = ~ ISI,
                title = "Initial Spread Index",
                opacity=0.7)
    })
  

  output$FWI = renderLeaflet({
    pal = colorBin(palette = "Okabe-Ito", 5, domain = master_sf_interp$FWI)
    
    date_filter() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "Esri.WorldImagery") %>%
      setView(-119.25, 49.25, zoom=10) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(date_filter()$FWI),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    fillOpacity = 1,
                    color = "black", 
                    opacity = 1, 
                    bringToFront = TRUE)) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = ~ FWI,
                title = "Fire Weather Index",
                opacity=0.7)
    
  })
  
  
  output$HFI = renderLeaflet({
    pal = colorBin(palette = "Okabe-Ito", 5, domain = master_sf_interp$HFI)
    
    date_filter() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "Esri.WorldImagery") %>%
      setView(-119.25, 49.25, zoom=10) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(date_filter()$HFI),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    fillOpacity = 1,
                    color = "black", 
                    opacity = 1, 
                    bringToFront = TRUE)) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = ~ HFI,
                title = "Head Fire Intensity (kW/m)",
                opacity=0.7)
    
  })
  
  
  output$RAZ = renderLeaflet({
    pal = colorBin(palette = "Tableau", 5, domain = master_sf_interp$RAZ)
    
    date_filter() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "Esri.WorldImagery") %>%
      setView(-119.25, 49.25, zoom=10) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(date_filter()$RAZ),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    fillOpacity = 1,
                    color = "black", 
                    opacity = 1, 
                    bringToFront = TRUE)) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = ~ RAZ,
                title = "Spread Azimuth Direction",
                opacity=0.7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
