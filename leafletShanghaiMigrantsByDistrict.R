#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("rgeos")

setwd("R_scripts/r_mapping/shanghaiR")


#load mapping and spatial data libraries
library(leaflet) 
library(rgdal)
library(rgeos)
library(classInt)
library(raster)

getwd() #get current R working directory. change with setwd("C:/Users/myusername")

nClassBreaks = 6
classifierFunction = "jenks"

#read in the GIS polygon shapefile of the adm1 data. Assumes data is in directory CHN_adm_shp
boundaries <- readOGR("../CHN_adm_shp/Shanghai_AL6_edit_for_Minhang.shp", layer = "Shanghai_AL6_edit_for_Minhang", verbose = FALSE)

#Geometry is quite detailed so they can be simplified to make them display quicker
#simplifiedPolygons <- gSimplify(boundaries, 0.001, topologyPreserve=TRUE) #Creates simplified polygons
simplifiedPolygons = boundaries
chinaSPDF = SpatialPolygonsDataFrame(simplifiedPolygons, data=boundaries@data) #Need to copy the attributes from the loaded data into 
simplifiedPolygons = spTransform(simplifiedPolygons, CRS("+proj=utm +zone=51 +datum=WGS84"))

chinaSPDF$areas = gArea(simplifiedPolygons,byid = TRUE)/1000000 #sqkm

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
chinaSPDF$migsNormalisedByArea = chinaSPDF$Number_of_migrants/chinaSPDF$areas
myVariableToMap2 = chinaSPDF$migsNormalisedByArea

#Classlify the data for choropleth mapping
palData = classIntervals(chinaSPDF$Number_of_migrants, n=nClassBreaks, style=classifierFunction)

palData2 = classIntervals(chinaSPDF$migsNormalisedByArea, n=nClassBreaks, style=classifierFunction)


#create a map palette style for the choropleth colour

#paletteColourFunction <- colorBin("YlOrRd", domain = myVariableToMap, bins = bins)
paletteColourFunction <- colorBin("Reds", domain = myVariableToMap, bins = palData$brks)
paletteColourFunction2 <- colorBin("Reds", domain = myVariableToMap2, bins = palData2$brks)






################### Map display labels ###############################

layerTitle = "Number of migrants"
layerTitle2 = "Number of migrants by area"

labels <- sprintf(
  "%s</strong><br/>%g migrants",
  chinaSPDF$Districts_and_Town, chinaSPDF$Number_of_migrants
) %>% lapply(htmltools::HTML)

labels2 <- sprintf(
  "%s</strong><br/>Density: %g migrants /km<sup>2</sup><br/>Num of migrants: %s",
  chinaSPDF$Districts_and_Town, chinaSPDF$migsNormalisedByArea, chinaSPDF$Number_of_migrants
) %>% lapply(htmltools::HTML)





######################## Map object creation ######################

# Put both datasets maps
myMap %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  
  addPolygons(
     fillOpacity = 0.60, smoothFactor = 0.5,
     fillColor = ~paletteColourFunction(myVariableToMap),
     weight = 2,
     dashArray = "1",
     color ="white",
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
    
  
  addLegend("bottomright", pal = paletteColourFunction, values = ~myVariableToMap,
            title = layerTitle,
            labFormat = labelFormat(prefix = ""),
            opacity = 1
  )%>%

  
  addLegend( "bottomright", pal = paletteColourFunction2, values = ~myVariableToMap2,
             title = layerTitle2,
             opacity = 1
  ) %>%

  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction2(myVariableToMap2),
    weight = 2,
    dashArray = "1",
    color = "white",
    group = layerTitle2,
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
    overlayGroups = c(layerTitle, layerTitle2),
    options = layersControlOptions(collapsed = FALSE)
  )
  #Viewer window should now display choropleth map.











myMapCounts = leaflet(chinaSPDF) %>% addTiles() 
# Separate map for counts
myMapCounts %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM basemap") %>%
  
  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction(myVariableToMap),
    weight = 2,
    color = "white",
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
  
  
  addLegend("bottomright", pal = paletteColourFunction, values = ~myVariableToMap,
            title = layerTitle,
            labFormat = labelFormat(prefix = ""),
            opacity = 1
  )

#Viewer window should now display choropleth map.










# Separate map for density
myMapDensity = leaflet(chinaSPDF) %>% addTiles() 
myMapDensity %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  
  addLegend( "bottomright", pal = paletteColourFunction2, values = ~myVariableToMap2,
             title = layerTitle2,
             opacity = 1
  ) %>%
  
  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction2(myVariableToMap2),
    weight = 2,
    dashArray = "1",
    color = "white",
    group = layerTitle2,
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
      direction = "auto")) 

#Viewer window should now display choropleth map.


