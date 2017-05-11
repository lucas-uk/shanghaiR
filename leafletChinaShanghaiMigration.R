#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("rgeos")

#load mapping and spatial data libraries
library(leaflet) 
library(rgdal)
library(rgeos)
library(classInt)

#Download the Shapefile of Administrative data for China from GADM:http://gadm.org/country
#unzip into a directory withing a suitable location e.g. R working directory. Find this out with:
getwd() #get current R working directory. change with setwd("C:/Users/myusername")


nClassBreaks = 6
classifierFunction = "jenks"


#read in the GIS polygon shapefile of the adm1 data. Assumes data is in directory CHN_adm_shp
china <- readOGR("../CHN_adm_shp/CHN_adm1.shp",
                  layer = "CHN_adm1", verbose = FALSE)

#Geometry is quite detailed so they can be simplified to make them display quicker
simplifiedPolygons <- gSimplify(china, 0.001, topologyPreserve=TRUE) #Creates simplified polygons
chinaSPDF = SpatialPolygonsDataFrame(simplifiedPolygons, data=china@data) #Need to copy the attributes from the loaded data into 
#chinaSPDF is a SpatialPolygonsDataFrame (polygones and attributes) that will be displayed on a map using Leaflet

simplifiedPolygons = spTransform(simplifiedPolygons, CRS("+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=mm +no_defs"))
proj4string(simplifiedPolygons)
chinaSPDF$areas = gArea(simplifiedPolygons,byid = TRUE) /1000000000000 #sq.mm to sqkm
sum(chinaSPDF$areas) #9382125.552585

#Create a map object with some background OpenStreetMap
myMap = leaflet(chinaSPDF) %>% addTiles() 
#myMap = leaflet(chinaSPDF) %>% addProviderTiles(providers$CartoDB.Positron)

#Determine a variable to map. Currently this is the ID_1 column of the gis polygons that is goint to be mapped. 
#Something else could be mapped by loading a table of data (e.g. migration counts), and joining to chinaSPDF using a common attribute.

#Read the csv of data to be mapped
migData = read.csv("../chinaCensusExtracts/province-shanghai_migration.csv", fill = TRUE)
#join (inner join) with merge
require(sp)
chinaSPDF <- merge(chinaSPDF,migData, by="ID_1")

myVariableToMap = chinaSPDF@data$Total

chinaSPDF$migsNormalisedByArea = chinaSPDF@data$Total/chinaSPDF$areas

myVariableToMap2 = chinaSPDF$migsNormalisedByArea


#Classlify the data for choropleth mapping
palData = classIntervals(migData$Total, n=nClassBreaks, style=classifierFunction)

palData2 = classIntervals(chinaSPDF$migsNormalisedByArea, n=nClassBreaks, style=classifierFunction)

#create a map palette style for the choropleth colour
pal <- colorNumeric(
  palette = "Greens",
  domain = myVariableToMap
)


#bins <- c(0, 10, 20, 50, 100, 20000, 50000, 100000, Inf)
#paletteColourFunction <- colorBin("YlOrRd", domain = myVariableToMap, bins = bins)
paletteColourFunction <- colorBin("Greens", domain = myVariableToMap, bins = palData$brks)

paletteColourFunction2 <- colorBin("Greens", domain = myVariableToMap2, bins = palData2$brks)




labels <- sprintf(
  "%s</strong><br/>%i migrants",
  chinaSPDF$NAME_1, chinaSPDF$Total
) %>% lapply(htmltools::HTML)

labels2 <- sprintf(
  "%s</strong><br/>Density: %g migrants /km<sup>2</sup><br/>Num of migrants: %s",
  chinaSPDF$NAME_1, chinaSPDF$migsNormalisedByArea, chinaSPDF$Total
) %>% lapply(htmltools::HTML)


layerTitle = "Number migrated to Shanghai<br>(Jenks breaks classification)"
layerTitle2 = "# of migrants to Shanghai by area (sq.km)<br>(Jenks breaks classification)"
#Add the polygons to the map object
myMap %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  
  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction(myVariableToMap),
    weight = 2,
    label=labels,
    group = layerTitle,
    dashArray = "2",
    color = "white",
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













#Add the polygons to the map object
myMap %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  
  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction2(myVariableToMap2),
    weight = 2,
    label=labels2,
    group = layerTitle2,
    dashArray = "2",
    color = "white",
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
  
  addLegend("bottomright", pal = paletteColourFunction2, values = ~myVariableToMap2,
            title = layerTitle2,
            labFormat = labelFormat(prefix = ""),
            opacity = 1
  )

#Viewer window should now display choropleth map.

