library(sf)
library(tidyverse)
library(uavRmp)
test<-"/Users/User/Documents/proj/mof/natur40/test.csv"
bu<-"/Users/User/Documents/proj/mof/natur40/cst_bu.kml"
ei<-"/Users/User/Documents/proj/mof/natur40/cst_ei.kml"
rt<-"/Users/User/Documents/proj/mof/natur40/Antennas.csv"
rts_csv<-  read_csv(rt)
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
vecDraw(overlay = rts)
fN<-"/Users/User/Documents/proj/mof/natur40/task1_rts_core.json"
fp<-makeAP(projectDir ="/Users/User/Documents/proj/mof/natur40/",
           locationName = "core10",
           surveyArea=fN,
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "/Users/User/Documents/proj/mof/natur40/geonode-lidar_dsm_01m.tif",
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
