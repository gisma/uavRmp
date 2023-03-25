## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----"setup", include=FALSE---------------------------------------------------
require("knitr")
knitr::opts_knit$set(root.dir = tempdir())

## ---- eval=FALSE--------------------------------------------------------------
#     require(uavRmp)
#     # get example DEM data
#     fn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#     fa <- system.file("extdata", "flightarea.kml", package = "uavRmp")
#     # preset = "uav" suppress all not necessary tools
#     vecDraw(mapCenter = c(50.855,8.691),preset="uav")
#  
#     # Use the digitized data (assumed to be named "firstSurvey.json")
#     # and the example DEM to calculate a flight control file
#     fp <- makeAP(surveyArea = fa,
#                  demFn = fn)
#  

## ---- eval=FALSE--------------------------------------------------------------
#   # load uavRmp
#   require(uavRmp)
#  
#   # start digitizing tool with preset = "uav" for a reduced toolbar
#   # see ?leafDraw for more information
#   vecDraw(mapCenter = c(50.855,8.691),preset="uav")

## ---- eval=FALSE--------------------------------------------------------------
#  
#    # get example DEM data
#   fn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#   fa <- system.file("extdata", "flightarea.kml", package = "uavRmp")
#  
#   fp<-makeAP(surveyArea=fa,
#              maxSpeed =35,
#              demFn = fn)

## ---- eval=FALSE--------------------------------------------------------------
#   require(mapview)
#   mapview(fp[[5]],color="red", alpha.regions =0.1,lwd=0.5)+
#   mapview(fp[[1]],lwd=1,cex=4)+
#   mapview(fp[[3]],color="red",cex=5)+
#   mapview(fp[[4]],color="darkblue", alpha.regions =0.1,lwd=0.5)

## ---- eval=FALSE--------------------------------------------------------------
#   useMP = TRUE

## ---- eval=FALSE--------------------------------------------------------------
#  demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#  tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey30m.plan", package = "uavRmp")
#  fp <- makeAP(projectDir = tempdir(),
#               surveyArea = tutorial_flightArea,
#               useMP = TRUE,
#               followSurface = TRUE,
#               above_ground = FALSE,
#               demFn = demFn,
#               uavType = "dji_csv"
#               )

