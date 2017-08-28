## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#   library(uavRmp)
#  
#   # preset = "uav" supress all not necessary tools
#   vecDraw(mapCenter = c(50.855,8.691),preset="uav")
#  
#   # Use the digitized data and the example DEM to calculate a flight control file
#   fp <- makeAP(projectDir = "~/proj",
#                locationName = "valleyWood",
#                surveyArea = "firstSurvey.json",
#                flightAltitude = 100,
#                demFn = data(mrbiko))

## ---- eval=FALSE---------------------------------------------------------
#  # load uavRmp
#  library(uavRmp)
#  
#  # start digitizing tool with preset = "uav" for a reduced toolbar
#  # see ?leafDraw for more information
#  vecDraw(mapCenter = c(50.855,8.691),preset="uav")

## ---- eval=FALSE---------------------------------------------------------
#  
#  data(mrbiko) # to use the example data it's easier to write same in tif format
#  
#  writeRaster(mrbiko,"~/dem.tif")
#  
#  fp<-makeFP(projectDir ="~/uav/proj",
#             missionName ="firstSurvey",
#             surveyArea="~/myFirstSurvey.json",
#             flightAltitude =100,
#             maxSpeed =35,
#             demFn ="~/dem.tif")

## ---- eval=FALSE---------------------------------------------------------
#  require(mapview)
#  mapview(fp[[5]],color="red", alpha.regions =0.1,lwd=0.5)+
#  mapview(fp[[1]],zcol ="altitude",lwd=1,cex=4)+
#  mapview(fp[[3]],color="red",cex=5)+
#  mapview(fp[[4]],color="darkblue", alpha.regions =0.1,lwd=0.5)

