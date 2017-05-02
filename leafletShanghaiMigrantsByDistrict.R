#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("rgeos")

#load mapping and spatial data libraries
library(leaflet) 
library(rgdal)
library(rgeos)
library(classInt)


getwd() #get current R working directory. change with setwd("C:/Users/myusername")


nClassBreaks = 6
classifierFunction = "jenks"


#read in the GIS polygon shapefile of the adm1 data. Assumes data is in directory CHN_adm_shp
boundaries <- readOGR("../CHN_adm_shp/Shanghai_AL6_edit_for_Minhang.shp", layer = "Shanghai_AL6_edit_for_Minhang", verbose = FALSE)

#Geometry is quite detailed so they can be simplified to make them display quicker
simplifiedPolygons <- gSimplify(boundaries, 0.001, topologyPreserve=TRUE) #Creates simplified polygons
chinaSPDF = SpatialPolygonsDataFrame(simplifiedPolygons, data=boundaries@data) #Need to copy the attributes from the loaded data into 

simplifiedPolygons = spTransform(simplifiedPolygons, CRS("+proj=utm +zone=51 +datum=WGS84"))

areas = gArea(simplifiedPolygons,byid = TRUE) #project, please

#Create a map object with some background OpenStreetMap
myMap = leaflet(chinaSPDF) %>% addTiles() 
#myMap = leaflet(chinaSPDF) %>% addProviderTiles(providers$CartoDB.Positron)

#Determine a variable to map. Currently this is the ID_1 column of the gis polygons that is goint to be mapped. 
#Something else could be mapped by loading a table of data (e.g. migration counts), and joining to chinaSPDF using a common attribute.

#Read the csv of data to be mapped
migData = read.csv("../chinaCensusExtracts/Shanghai_migrants_by_district_merged_for_2017_osm_districts.csv", fill = TRUE)
#join (inner join) with merge
require(sp)
chinaSPDF <- merge(chinaSPDF,migData, by.x="ID", by.y="OSMID")

#Count variable
myVariableToMap = chinaSPDF@data$Number_of_migrants
#Add a normalised by area variable
migData$migsNormalisedByArea = migData$Number_of_migrants/areas
myVariableToMap2 = migData$migsNormalisedByArea

#Classlify the data for choropleth mapping
palData = classIntervals(migData$Number_of_migrants, n=nClassBreaks, style=classifierFunction)

palData2 = classIntervals(migData$migsNormalisedByArea, n=nClassBreaks, style=classifierFunction)


#create a map palette style for the choropleth colour
pal <- colorNumeric(
  palette = "Greens",
  domain = myVariableToMap
)

pal2 <- colorNumeric(
  palette = "Greens",
  domain = myVariableToMap2
)


#paletteColourFunction <- colorBin("YlOrRd", domain = myVariableToMap, bins = bins)
paletteColourFunction <- colorBin("Reds", domain = myVariableToMap, bins = palData$brks)
paletteColourFunction2 <- colorBin("Reds", domain = myVariableToMap2, bins = palData2$brks)



layerTitle = "Number of migrants"
layerTitle2 = "Number of migrants by area"

labels <- sprintf(
  "<strong>%s</strong><br/>%g migrants",
  migData$Districts_and_Town, migData$Number_of_migrants
) %>% lapply(htmltools::HTML)

myMap %>%
  #addProviderTiles("Stamen.Toner") %>%
  #addProviderTiles("CartoDB.Positron") %>%
  #addProviderTiles("ESRI.WorldImagery") %>%
  
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM basemap (CartoDB Positron)") %>%
  
  addPolygons(
     fillOpacity = 0.60, smoothFactor = 0.5,
     fillColor = ~paletteColourFunction(myVariableToMap),
     weight = 2,
     dashArray = "1",
     group = layerTitle,
     label=labels,
     highlight = highlightOptions(
       weight = 5,
       color = "#666",
       dashArray = "",
       fillOpacity = 0.7,
       bringToFront = TRUE),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%  
    
  
  addLegend("topright", pal = paletteColourFunction, values = ~myVariableToMap,
            title = layerTitle,
            labFormat = labelFormat(prefix = ""),
            opacity = 1
  )%>%

  
  addLegend( pal = paletteColourFunction2, values = ~myVariableToMap2,
             title = layerTitle2,
             opacity = 1
  ) %>%

  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction2(myVariableToMap2),
    weight = 2,
    dashArray = "1",
    color = "white",
    group = layerTitle,
    label=labels,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%  

  
  addLayersControl(
    #baseGroups = c("OSM basemap (CartoDB Positron)", "OSM basemap"),
    overlayGroups = c(layerTitle, layerTitle2),
    options = layersControlOptions(collapsed = FALSE)
  )


  #Viewer window should now display choropleth map.















labels2 <- sprintf(
  "<strong>%s</strong><br/>%g migrants / m<sup>2</sup>",
  migData$Districts_and_Town, migData$migsNormalisedByArea
) %>% lapply(htmltools::HTML)


myMap2 = leaflet(chinaSPDF) %>% addTiles() 

myMap2 %>%
  #addProviderTiles("Stamen.Toner") %>%
  #addProviderTiles("CartoDB.Positron") %>%
  #addProviderTiles("ESRI.WorldImagery") %>%
  
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM basemap (CartoDB Positron)") %>%

  addLegend( pal = paletteColourFunction2, values = ~myVariableToMap2,
             title = layerTitle2,
             opacity = 1
  ) %>%
  
  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction2(myVariableToMap2),
    weight = 2,
    dashArray = "1",
    color = "white",
    group = layerTitle,
    label=labels2,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%  
  
  
  addLayersControl(
    #baseGroups = c("OSM basemap (CartoDB Positron)", "OSM basemap"),
    options = layersControlOptions(collapsed = FALSE)
  )


#Viewer window should now display choropleth map.
