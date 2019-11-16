library(sf)
library(tidyverse)
library(uavRmp)
test<-"~/temp3/test2.plan"
test2<-"~/temp3/flight.json"
bu<-"/Users/User/Documents/proj/mof/natur40/cst_bu.kml"
ei<-"/Users/User/Documents/proj/mof/natur40/cst_ei.kml"
rt<-"~/temp3/project1/Antennas.csv"
rts_csv<-  read_csv(rt)

t<-jsonlite::fromJSON(test)
df<- t$mission$items$TransectStyleComplexItem$Items[2][[1]]
df$params[[1]][5:6]
sf::st_read(test,drivers="GeoJSON")

rts <- st_as_sf(
  rts_csv, 
  coords = c('Longitude', 'Latitude'),
  crs = "+init=epsg:4326"
)
sf <- sf::geojson_sf(rts_csv)
rt<-as(rts, 'Spatial')
survey <- sf::st_read(path.expand(fN))
fagus <- sf::st_read(path.expand(bu))
quercus <- sf::st_read(path.expand(ei))

flightBound = as(survey, "Spatial")
fpa <- as(fagus, "Spatial")
q <- as(quercus, "Spatial")
s<-q+fpa+rt
vecDraw(overlay = rt)
fN<-"~/temp3/south.json"
fp<-makeAP(projectDir ="~/temp3/project1/",
           locationName = "core10",
           surveyArea=fN,
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "/home/creu/temp3/project1/core10/dsm.tif",
           windCondition = 1,
           followSurfaceRes = 25,
           maxSpeed = 40,
           uavType = "solo"
           
)

mapview::mapview(fp$wp,cex=4, lwd=0.5)+
  mapview::mapview(fp$lp,color = "red", lwd=1,cex=4)+
  mapview::mapview(fp$fA,color="blue", alpha.regions = 0.1,lwd=0.5)+
  
  mapview::mapview(fagus)+
  mapview::mapview(quercus)

