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

#Classlify the data for choropleth mapping
palData = classIntervals(migData$Total, n=nClassBreaks, style=classifierFunction)

#create a map palette style for the choropleth colour
pal <- colorNumeric(
  palette = "Greens",
  domain = myVariableToMap
)


#bins <- c(0, 10, 20, 50, 100, 20000, 50000, 100000, Inf)
#paletteColourFunction <- colorBin("YlOrRd", domain = myVariableToMap, bins = bins)
paletteColourFunction <- colorBin("Greens", domain = myVariableToMap, bins = palData$brks)


#Add the polygons to the map object
myMap %>%
  addPolygons(
    fillOpacity = 0.60, smoothFactor = 0.5,
    fillColor = ~paletteColourFunction(myVariableToMap),
    weight = 2,
    dashArray = "2",
    color = "white"
  ) %>%
  addLegend("topright", pal = paletteColourFunction, values = ~myVariableToMap,
          title = "Migration to Shanghai",
          labFormat = labelFormat(prefix = ""),
          opacity = 1
  )
#Viewer window should now display choropleth map.




