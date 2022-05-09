library(shiny)
library(leaflet)

ui <- fluidPage(
  
  # Application title
  titlePanel("CabinGIS Wildfire Fuel + Weather Platform"),
  
  sidebarLayout(
    sidebarPanel(),
    
    mainPanel(
      leafletOutput("CabinGIS Wildfire Fuel + Weather Platform")
    )
  )
)

server <- function(input, output) {
  
  output$map<-renderLeaflet({
    leaflet() %>% 
      addWMSTiles( "https://image.discomap.eea.europa.eu/arcgis/services/Corine/CLC2018_WM/MapServer/WmsServer", layers = "1",
                   options = WMSTileOptions(format = "image/png", transparent = T)) %>% 
      setView(lng = -1.8, lat = 52.0, zoom = 5)
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
