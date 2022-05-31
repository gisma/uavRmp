if (!isGeneric('makeAP')) {
  setGeneric('makeAP', function(x, ...)
    standardGeneric('makeAP'))
}
#'UAV Mission Planning tool for autonomous monitoring flight tasks with respect to DSM/DEM, orthophoto data retrieval.
#'
#' @description The basic idea is to provide an easy to use workflow for controlling rtf-UAVs for planning autonomous surveys to retrieve aerial data sets. 
#'   
#' @details makeAP (make aerial plan) creates either intermediate flight control files for the
#'   DJI phantom x UAVs or ready to upload control files for the 3DR Solo/PixHawk flightcontroller. The
#'   DJI control files are designed for using with the proprietary litchi flight
#'   control app exchange format, while the 3DR Solo/PixHawk flightcontroller files are using the MAVLINK
#'   common message set, that is used by the PixHawk flight controller family.
#'   Both are implemented very rudimentarily.\cr\cr DJI:\cr The reason using DJI
#'   is their absolute straightforward usage. Everybody can fly with a DJI but
#'   the price is a more or less closed system at least in the low budget segement. There are workarounds like the litchi app that provides
#'   additionally to a cloud based mission planner an offline/standalone
#'   interface to upload a CSV formated way point file for autonomous flights to
#'   the Phantom.\cr\cr PixHawk flightcontroller/3DR Solo:\cr The open uav community is focused
#'   on the PixHawk autopilot unit and the Mission Planner software. It is well
#'   documented and several APIs are provided. Nevertheless a high resolution 
#'   terrain following flight planning tool for  autonomous obstacle avoiding flight missions
#'   is not available. `makeAP` creates a straightforward version of MAV format flight control
#'   rules that are ready to be uploaded directly on the Pixhawk controller using the `solo_upload` function.\cr\cr
#'   
#' @seealso
#'   The underlying concept, a tutorial and a field guide can be found in the package vignettes. See `browseVignettes("uavRmp")` or `vignette(package =
#'   "uavRmp")` or 
#'   at [Github uavRmp manual](https://gisma.github.io/uavRmp/articles/uavRmp_1.html)).
#'

#' @section Warning: Take care! There are still a lot of construction zones
#'   around. This script is far beyond to be in a mature state. Please control
#'   and backup all controls again while planning and performing autonomous
#'   flight plans and missions. You will have a lot of chances to make a small
#'   mistake what may yield in a damage of your UAV or even worse in involving
#'   people, animals or non-cash assets. Check your risk, use parachute systems
#'   and even if it is running like a charm, keep alert!


#' @param projectDir `character` path to the main folder where several locations can be hosted, default is `tempdir()`
#' @param locationName `character` path to the location folder where all tasks of this plot are hosted, default is `"flightArea"` 
#' @param surveyArea  you may provide either the coordinates by
#' c(lon1,lat1,lon2,lat2,lon3,lat3,launchLat,launchLon) or
#' an OGR compatible file (prefunable to find an inherited method for function ‘makeAP’ for signature ‘"missing"’erably geoJSON or KML) with
#' at least 4 coordinates that describe the flight area.
#' The fourth coordinate is the launch position.
#'  You will find further explanation under seealso.
#' @param launchAltitude absolute altitude of launching position.
#' It will overwrite the DEM based estimation if any other value than -9999
#' @param demFn  filename of the corresponding DEM data file.

#' @param followSurface  `boolean`  TRUE performs an altitude correction
#' of the mission's flight altitude using additional DEM data.
#' If no DEM data is provided and `followSurface` is TRUE,
#' SRTM data will be downloaded and used.
#' Further explanation at seealso
#' @param altFilter if `followingTerrain` is equal `TRUE` then
#' `altFilter` is the threshold value of accepted altitude difference (m) between two way points.
#'  If this value is not exceeded, the way point is omitted due to the fact that only 99 way points per mission are allowed.
#' @param horizonFilter integer filter size of the rolling filter kernel for the flight track. Must be multiplied by the `followSurfaceRes` to get the spatial extent
#' @param flightPlanMode type of flight plan. Available are: `"waypoints"`,
#'   `"track"`, `"manual"`.
#' @param useMP default is FALSE switches to use a missionplanner/Qgroundcontrolplanner survey as planning base
#' @param presetFlightTask (DJI only) strongly recommended to use "remote"
#'        \cr
#'  Options are:
#' `"simple_ortho"` takes one picture/way point,
#' `"multi_ortho"` takes 4 picture at a waunable to find an inherited method for function ‘makeAP’ for signature ‘"missing"’unable to find an inherited method for function ‘makeAP’ for signature ‘"missing"’ypoint, two vertically down and two in forward and backward viewing direction and an Angele of -60deg,
#' `"simple_pano"` takes a 360 deg panorama picture and
#' `"remote"` which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude set the default flight altitude of the mission. It is
#'   assumed that the UAV is started at the highest point of the surveyArea
#'   otherwise you have to defined the position of launching.
#' @param overlap overlapping of the pictures in percent (1.0 = 100)
#' @param djiBasic c(0,0,0,-90)
#' \cr curvesize (DJI only) controls the curve angle of the uav passing way points.
#' By default it is set to (`= 0.0`).
#' \cr rotationdir (DJI only) camera control parameter set the UAV basic turn direction to right (0) or left (1)
#' \cr gimbalmode (DJI only) camera control parameter
#' `0` deactivates the gimbal control
#' `1` activates the gimbal for focusing POIs
#' `2` activates the gimbal for focus and interpolate a field of view in an angel of `gimbalpitchangle`
#' \cr gimbalpitchangle (DJI only) vertical angle of camera  `+30 deg..-90 deg`
#' \cr actiontype (DJI only) individual actionype settings of the camera c(1,1,...)
#' \cr actionparam (DJI only) corresponding parameter for the above individual actiontype c(0,0,...)
#' `uavViewDir` viewing direction of camera default is `0`
#' @param maxSpeed  cruising speed
#' @param heatMap switch for calculating the overlapping factor on a raster map
#' @param picFootprint switch for calculating the footprint at all way points
#' @param followSurfaceRes horizontal step distance for analyzing the DEM altitudes
#' @param picRate fastest stable interval (s) for shooting pictures
#' @param windCondition 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h
#' @param copy copy switch
#' @param cmd mavlink command
#' @param uavViewDir dview direction of uav
#' @param maxFlightTime user defined estimation of the lipo lifetime (20 min default)
#' @param rcRange range of estimated range of remote control
#' @param uavType type of uav. currently "dji_csv" and "solo" are supported
#' @param dA if TRUE the real extent of the used DEM is returned helpful for low altitudes flight planning
#' @param cameraType depending on uav system for dji the dji4k is default for solo you can choose GP3_7MP GP3_11MP and MAPIR2
#' @param runDir `character` runtime folder 
#' @param gdalLink link to GDAL binaries
#'
#' @examples
#'\dontrun{
#' # Depending on the arguments, the following spatial data sets can be returned:
#'
#' # lp      the planned launching position of the UAV.
#' # wp      waypoints inclusive all information
#' # oDEM    the original (input) digital surface model (DSM)
#' # rDEM    the resampled (used) DSM
#' # fp      optimized footprints of the camera
#' # fA      flight area with at least 2 overlaps
#' # rcA     area covered by the RC according to the range and line of sight
#' # hm      a heatmap abundance of pictures/pixel (VERY SLOW, only if heatMap = TRUE)
#'
#' ## for visualisation and vecDraw load mapview
#' require(mapview)
#'
#' ## (1) get example DEM data
#' demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#' tutorial_flightArea <- system.file("extdata", "flightarea.kml", package = "uavRmp")
#' 
#' ## (2) simple flight, 100 meters above ground
#' ##     assuming a flat topography,
#'
#' fp <- makeAP(surveyArea = tutorial_flightArea,
#'               demFn = demFn)
#'               
#' ## (3) typical real case scenario (1)
#' ##     A flight altitudes BELOW 50 m is ambitious and risky
#' ##     You have to use a high quality high resulution DSM
#' ##     (here simulated with a standard DEM)
#'
# fp <- makeAP(surveyArea=tutorial_flightArea,
#            followSurface = TRUE,
#            flightAltitude = 45,
#            demFn = demFn,
#            windCondition = 1,
#            uavType = "pixhawk",
#            followSurfaceRes = 5,
#            altFilter = .75)
#'
#'
#' ## (4) typical real case scenario (2)
#' ##     A flight altitudes BELOW 50 m is ambitious and risky
#' ##     You have to use a high quality high resulution DSM
#' ##     (here simulated with a standard DEM)
#' ##     This examples uses a flight planning from the QGroundcotrol Survey planning tool
#' ##     It also used the all calculations for camera flight speed etc.
#' ##     NOTE EXPERIMENTAL 
#' 
#'demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#'tutorial_flightArea <- system.file("extdata", "qgc_survey.plan", package = "uavRmp")
#'fp <- makeAP(surveyArea=tutorial_flightArea,
#'             useMP = TRUE,
#'             followSurface = TRUE,
#'             demFn = demFn,
#'             windCondition = 1,
#'             uavType = "pixhawk",
#'             followSurfaceRes = 5,
#'              altFilter = .75)
#'
#' ## (5) typical real case scenario (3)
#' ##     This examples uses a flight planning from the QGroundcotrol Survey planning tool
#' ##     It also used the all calculations for camera flight speed etc.
#' ##     The flight plan is modyfied by splitting up the task according to 99 Waypoints
#' ##     and flight time and saved as litchi csv format 
#' ##     NOTE EXPERIMENTAL tested with DJI mavic mini 2
#' 
#'demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#'tutorial_flightArea <- system.file("extdata", "qgc_survey.plan", package = "uavRmp")
#'fp <- makeAP(surveyArea=tutorial_flightArea,
#'             useMP = TRUE,
#'             demFn = demFn,
#'             maxFlightTime = 25,
#'             uavType = "dji_csv")
#'             
#' ## call a simple shiny interface
#' runApp(system.file("shiny/plan2litchi/", "app.R", package = "uavRmp"))
#'         
#'         
#' ## (6) view results
#' 
#'mapview::mapview(fp$wp,cex=4, lwd=0.5)+
#'mapview::mapview(fp$lp,color = "red", lwd=1,cex=4)+
#'mapview::mapview(fp$fA,color="blue", alpha.regions = 0.1,lwd=0.5)+
#'mapview::mapview(fp$oDEM,col=terrain.colors(256))
#'
#'
#'
#' ## (6) digitize flight area using the small "onboard" tool vecDraw()
#' ##     save vectors as "kml" or "json" files
#' ##     provide full filename  +  extension!
#' 
#' 
#' vecDraw(preset="uav")
#'}






#' @export makeAP
#'


makeAP <- function(projectDir = tempdir(),
                   locationName = "flightArea",
                   surveyArea = NULL,
                   flightAltitude = 100,
                   launchAltitude = NULL,
                   followSurface = FALSE,
                   followSurfaceRes = 25,
                   demFn = NULL,
                   altFilter = 1.0,
                   horizonFilter = 30,
                   flightPlanMode = "track",
                   useMP = FALSE,
                   presetFlightTask = "remote",
                   overlap = 0.8,
                   maxSpeed = 20.0,
                   maxFlightTime = 10,
                   picRate = 2,
                   windCondition = 0,
                   uavType = "pixhawk",
                   cameraType = "MAPIR2",
                   cmd=16,
                   uavViewDir = 0,
                   djiBasic = c(0, 0, 0,-90, 0),
                   dA = FALSE,
                   heatMap = FALSE,
                   picFootprint = FALSE,
                   rcRange = NULL,
                   copy = FALSE,
                   runDir=tempdir(),
                   gdalLink=NULL)
{
  ###  setup environ and params
  cat("setup environ and params...\n")
  # assign flight mission name
  #locationName <- file.path(locationName,"missions")
  if (substr(projectDir,nchar(projectDir),nchar(projectDir)) == "/")  projectDir <- substr(projectDir,1,nchar(projectDir)-1)
  else if (substr(projectDir,nchar(projectDir),nchar(projectDir)) == "\\") projectDir <- substr(projectDir,1,nchar(projectDir)-1)
  projstru <- setProjStructure (projectDir,
                                locationName, 
                                flightAltitude,
                                uavType,
                                cameraType,
                                surveyArea,
                                demFn,
                                copy)
  
  dateString <- projstru[3]
  taskName <- projstru[2]
  csvFn <- projstru[1]

  ## create log file
  logger <- log4r::create.logger(logfile = paste0(file.path(projectDir, locationName, dateString, "fp-data/log/"),strsplit(basename(taskName[[1]]), "\\.")[[1]][1],'.log'))
  log4r::level(logger) <- "INFO"
  log4r::levellog(logger,'INFO',"--------------------- START RUN ---------------------------")
  log4r::levellog(logger, 'INFO', paste("Working folder: ", file.path(projectDir, locationName, dateString)))
  
  ## need picfootprint for calculating the heatmap
  if (heatMap) { picFootprint = TRUE }
  
  ## uav platform depending parameter setting
  if (uavType == "dji_csv") {
    #browser()
    cameraType<-"dji4k"
    factor <- 1.71 # FOV ratio
    flightParams = c(flightPlanMode = flightPlanMode,
                     launchAltitude = launchAltitude,
                     flightAltitude = flightAltitude,
                     presetFlightTask = presetFlightTask,
                     overlap = overlap,
                     curvesize = djiBasic[1],        # curvesize
                     rotationdir = djiBasic[2],      # rotationdir
                     gimbalmode = djiBasic[3],       # gimbalmode
                     gimbalpitchangle = djiBasic[4], # gimbalpitchangle
                     uavViewDir = uavViewDir,
                     followSurfaceRes = followSurfaceRes)
    
    #calc & assign overlapping factor as a function of flightAltitude
    fliAltRatio <- 1 - overlap
    
    # FOV*agl*(1-overlap)
    uavOptimumSpeed <- ceiling(factor * flightAltitude * fliAltRatio)
    
  } 
  else if (uavType == "pixhawk") {
    if (cameraType == "MAPIR2") {
      factor <- 1.55
    } else if (cameraType == "GP3_7MP") {
      factor <- 1.31
    } else if (cameraType == "GP3_11MP") {
      factor <-1.71
    }
  
    flightParams = c(flightPlanMode = flightPlanMode,
                     launchAltitude = launchAltitude,
                     flightAltitude = flightAltitude,
                     presetFlightTask = presetFlightTask,
                     overlap = overlap,
                     uavViewDir = uavViewDir,
                     followSurfaceRes = followSurfaceRes)
    
    #calc & assign overlapping factor as a function of flightAltitude
    fliAltRatio <- 1 - overlap
    
    # FOV*agl*(1-overlap)
    uavOptimumSpeed <- ceiling(factor * flightAltitude * fliAltRatio)
  }
  
  #-----------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------
  # if a missionplanner surveypalanning is used 
  if (useMP) {
    
    t<-jsonlite::fromJSON(surveyArea)
    listPos<-grep("command", t$mission$items$TransectStyleComplexItem$Items)
    tmp<- t$mission$items$TransectStyleComplexItem$Items[listPos][[1]]
    #length(tmp$params[[60]])
    #tmp$params[[1]][5:6]
    coord<-tmp[tmp["command"]==16, ]
    #coord$params
    df_coordinates<-t(as.data.frame(rlist::list.cbind(coord[,"params",])))[,5:6]
    # t$mission$items$TransectStyleComplexItem$VisualTransectPoints
    tracks<- ceiling(nrow(coord)/4)
    trackDistance <- t$mission$items$TransectStyleComplexItem$CameraCalc$AdjustedFootprintFrontal[listPos]
    crossDistance   <- t$mission$items$TransectStyleComplexItem$CameraCalc$AdjustedFootprintSide[listPos]
    totalTrackdistance <- trackDistance
    fliAltRatio     <- 1 - t$mission$items$TransectStyleComplexItem$CameraCalc$SideOverlap[listPos]/100
    flightAltitude  <- t$mission$items$TransectStyleComplexItem$CameraCalc$DistanceToSurface[listPos]
    #maxSpeed        <-  t$mission$items$TransectStyleComplexItem$TerrainFlightSpeed[3]*3.6 #t$mission$cruiseSpeed
    if (length(t$mission$items$TransectStyleComplexItem$TerrainFlightSpeed[3]*3.6)!=0) maxSpeed <-  t$mission$items$TransectStyleComplexItem$TerrainFlightSpeed[3]*3.6
    launchLat       <- t$mission$plannedHomePosition[1]
    launchLon       <- t$mission$plannedHomePosition[2]
    updir           <- t$mission$items$angle[listPos]
    if (updir <= 180) downdir <- updir + 180
    else if (updir>180) downdir<- updir -180
    crossdir        <- geosphere::bearing(c(df_coordinates[2,][2],df_coordinates[2,][1] ),c(df_coordinates[3,][2],df_coordinates[3,][1] ),a = 6378137,f = 1 / 298.257223563)
    missionArea     <- t$mission$items$polygon[listPos]
    # calculate heading from launch position to mission start position
    launch2startHeading <- geosphere::bearing(p1 = c(launchLon, launchLat),p2 = c(df_coordinates[1,][2],df_coordinates[1,][1] ),a = 6378137,f = 1 / 298.257223563)
    groundResolution<-t$mission$items$TransectStyleComplexItem$CameraCalc$ImageDensity[listPos]
    
    # set cumulative flightlength to zero
    flightLength <- 0
    if (uavType == "dji_csv") 
    flightParams = c(flightPlanMode = flightPlanMode,
                     launchAltitude = launchAltitude,
                     flightAltitude = flightAltitude,
                     presetFlightTask = presetFlightTask,
                     overlap = overlap,
                     curvesize = djiBasic[1],        # curvesize
                     rotationdir = djiBasic[2],      # rotationdir
                     gimbalmode = djiBasic[3],       # gimbalmode
                     gimbalpitchangle = djiBasic[4], # gimbalpitchangle
                     uavViewDir = uavViewDir,
                     followSurfaceRes = followSurfaceRes)
    if (uavType == "pixhawk") 
    flightParams = c(flightPlanMode = flightPlanMode,
                     launchAltitude = launchAltitude,
                     flightAltitude = flightAltitude,
                     presetFlightTask = presetFlightTask,
                     overlap = 1- fliAltRatio ,
                     uavViewDir = uavViewDir,
                     followSurfaceRes = followSurfaceRes)
    
    p <- makeFlightParam( c(missionArea[[1]][1],missionArea[[1]][5],
                            missionArea[[1]][2],missionArea[[1]][6] ,
                            missionArea[[1]][3],missionArea[[1]][7] ,
                            launchLat, launchLon),
                          flightParams, followSurface)
    mode<-p$flightPlanMode
    
    
    # set universal view direction of the uav
    if (abs(as.numeric(flightParams["uavViewDir"])) == 0) {
      uavViewDir <- updir
    }
    else {
      uavViewDir <- abs(as.numeric(flightParams["uavViewDir"]))
    }    
    ## calculate survey area
    # create an sp polygon object of the mission area
    # your data (removed crs column)
   # browser()
    tarea <- data.table::data.table(
      longitude= as.data.frame(t$mission$items$polygon[listPos][1])[,2],
      latitude=as.data.frame(t$mission$items$polygon[listPos][1])[,1])
    tarea = sf::st_as_sf(tarea, coords = c("longitude", "latitude"), 
                         crs = 4326)
    tarea<- sf::st_bbox(tarea)
    taskArea<-sf::st_as_sfc(sf::st_bbox(tarea))
    taskAreaUTM <- sf::st_transform(taskArea, 4326)
    # reproject it to UTM
    #taskAreaUTM <- sp::spTransform(taskArea, sp::CRS(paste("+proj=utm +zone=",long2UTMzone(p$lon1)," ellps=WGS84",sep = '')))
    # calculate area
    surveyAreaUTM <- sf::st_area(taskAreaUTM)
    #########################################
    #########################
    # initialize jiDF and mav
    djiDF <- data.frame()
    mavDF <- data.frame()
    #set initial heading
    heading <- updir
    # define output line var
    lns <- list()
    lns <- launch2flightalt(p, lns, uavViewDir, launch2startHeading, uavType)
    # assign starting point
    pos <- c(df_coordinates[1,][2],df_coordinates[1,][1])
    
    footprint <- calcCamFoot(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0,factor)
    footprint<-  sp::spTransform(footprint,sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    landscape<-abs(abs(footprint@bbox[1]-footprint@bbox[3])*overlap-abs(footprint@bbox[1]-footprint@bbox[3]))
    portrait<- abs(abs(footprint@bbox[2]-footprint@bbox[4])*overlap-abs(footprint@bbox[2]-footprint@bbox[4]))
    
    
    # calculates the footprint of the first position and returns a SpatialPolygonsDataFrame
    if (picFootprint)  camera <- calcCamFoot(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0,factor)
    else  camera = "NULL"
    
    # ## creates the export control parameter set of the first position
    # if (uavType == "dji_csv") {
    #  lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
    # }
    #
    if (uavType == "pixhawk") {
      lns[length(lns) + 1] <-  makeUavPointMAV(lat = pos[2],lon = pos[1], head = uavViewDir, group = 99 )
    }
    # push pos to old pos
    pOld <- pos
    if (uavType == "pixhawk") {
# set counter and params for mode = "track" mode
    if (mode == "track") {
      if (uavType == "dji_csv") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      }
      if (uavType == "pixhawk") {
        lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
      }
#      trackDistance <- len
      multiply <- 1
    }

    ## set counter and params for mode = "waypoints"
    else if (mode == "waypoints") {
      if (uavType == "dji_csv") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      }
      if (uavType == "pixhawk") {
        lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
      }
    }

    ## set counter and params for mode = "terrainTrack"
    else if (mode == "terrainTrack")
      group = 99 }
    group = 99
    df_coord<-as.data.frame(df_coordinates)
    names(df_coord)<-c("lat","lon")
    for (j in 2:(nrow(df_coord)-1)) {
      df_coord$heading[j] <- geosphere::bearing(c(df_coord$lon[j],df_coord$lat[j] ), c(df_coord$lon[j + 1],df_coord$lat[j + 1]),a = 6378137,f = 1 / 298.257223563)
      df_coord$len[j] <- geosphere::distGeo(c(df_coord$lon[j],df_coord$lat[j] ), c(df_coord$lon[j + 1],df_coord$lat[j + 1]),a = 6378137,f = 1 / 298.257223563)
      df_coord$multiply[j] <- floor(df_coord$len[j] / followSurfaceRes)
     }
    ## now start calculating the waypoints according to the resolution
    cat("calculating waypoints...\n")
    pb <- pb <- utils::txtProgressBar(max = tracks, style = 3)
    # then do for the rest  forward and backward
    for (j in 2:(nrow(df_coord)-1)) {
      pOld<- c(df_coord$lon[j],df_coord$lat[j])
     # for (i in seq(1:df_coord$multiply[j])) {
        if (mode == "waypoints" || mode == "terrainTrack" || mode == "track") {
          group <- 1
          if (df_coord$multiply[j] < 1 || is.na(df_coord$multiply[j])) {group <- 99}}
        #  else      {group <- 1}}
        #else {i <- 2}
        # calc next coordinate
        
        pos <- calcNextPos(pOld[1], pOld[2], df_coord$heading[j], followSurfaceRes)
        
        pOld <- pos

        flightLength <- flightLength + followSurfaceRes
        
        if (mode == "track") {
          group <- 99
        }
        if (uavType == "dji_csv") {
          lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
        }
        if (uavType == "pixhawk") {
        lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2], lon = pos[1], head = uavViewDir, group = group)
        }
      #}
      
      # if ((j %% 2 != 0)) {
      #   dir<- geosphere::bearing(c(df_coordinates[j ,][2],df_coordinates[j ,][1]) , c(df_coordinates[j + 1,][2],df_coordinates[j + 1,][1] ),a = 6378137,f = 1 / 298.257223563)
      #   pos <- calcNextPos(pOld[1], pOld[2] , heading = dir, distance = trackDistance)
      #   if (picFootprint) camera <-  spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir, trackDistance,flightAltitude,i,j))
      #   pOld <- pos
      #   flightLength <- flightLength + crossDistance
      #   if (uavType == "dji_csv") {
      #     lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      #   }
      #   if (uavType == "pixhawk") {
      #     lns[length(lns) + 1] <-
      #       makeUavPointMAV(
      #         lat = pos[2],
      #         lon = pos[1],
      #         head = uavViewDir,
      #         group = 99
      #       )
      #   }
      #   heading <- downdir
      # }
      # 
      # else if ((j %% 2 == 0)) {
      #   dir<- geosphere::bearing(c(df_coordinates[j ,][2],df_coordinates[j ,][1]) , c(df_coordinates[j + 1,][2],df_coordinates[j + 1,][1] ),a = 6378137,f = 1 / 298.257223563)
      #   pos <- calcNextPos(pOld[1], pOld[2], heading = dir, distance = trackDistance)
      #   if (picFootprint) camera <- spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir,trackDistance,flightAltitude,i,j))
      #   pOld <- pos
      #   flightLength <- flightLength + crossDistance
      #   
      #   if (uavType == "dji_csv") {
      #     lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      #     heading <- updir
      #   }
      #   if (uavType == "pixhawk") {
      #     lns[length(lns) + 1] <-  makeUavPointMAV( lat = pos[2], lon = pos[1], head = uavViewDir - 180, group = 99)
      #     heading <- updir
      #   }
      #   
      # }
      # status bar
      utils::setTxtProgressBar(pb, j)
    }
    close(pb)    
  }
  #-----------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------
  
  else if (!useMP){
  ## get survey area
  ##
  surveyArea <- calcSurveyArea(surveyArea, projectDir, logger, useMP)
  

  # adapt default flight params to runtime request
  p <- makeFlightParam(surveyArea, flightParams, followSurface)
  
  # assign flightmode
  mode <- as.character(p$flightPlanMode)
  
  # assign flight Altitude
  flightAltitude <- as.numeric(flightParams["flightAltitude"])
  
  
  # calc distance between two pictures using a camera dependent multiplicator
  trackDistance <- calcTrackDistance(fliAltRatio, flightAltitude, factor)
  totalTrackdistance <- trackDistance
  # pictures are assumed as squares
  crossDistance <- trackDistance
  

  ## calculate survey area
  # create an sp polygon object of the mission area
  taskArea <- taskarea(p, csvFn)
  mapview::mapview( taskArea)
  # reproject it to UTM
  taskAreaUTM <- sp::spTransform(taskArea, sp::CRS(paste("+proj=utm +zone=",long2UTMzone(p$lon1)," ellps=WGS84",sep = '')))
  # calculate area
  surveyAreaUTM <- rgeos::gArea(taskAreaUTM)
  
  
  ## now do old planning stuff
  # calculate heading from launch position to mission start position
  launch2startHeading <- geosphere::bearing(c(p$launchLon, p$launchLat),c(p$lon1, p$lat1),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track W-E
  updir <- geosphere::bearing(c(p$lon1, p$lat1),c(p$lon2, p$lat2),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track E-W
  downdir <- geosphere::bearing(c(p$lon2, p$lat2),c(p$lon1, p$lat1),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track trackline to trackline
  crossdir <- geosphere::bearing(c(p$lon2, p$lat2),c(p$lon3, p$lat3),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  distance of the base flight track
  len <- geosphere::distGeo(c(p$lon1, p$lat1), c(p$lon2, p$lat2))
  
  # calculate and assign distance of the cross base flight track
  crosslen <- distGeo(c(p$lon2, p$lat2),c(p$lon3, p$lat3),a = 6378137,f = 1 / 298.257223563)
  
  if (is.null(followSurfaceRes)) {
    followSurfaceRes <- trackDistance
    p$followSurfaceRes <- followSurfaceRes
  }
  
  # IF followSurface set track/crossDistance to followSurfaceRes
  if (followSurface) {
    multiply <- floor(len / followSurfaceRes)
    trackDistance <- followSurfaceRes
    #crossDistance<-followSurfaceRes
  } else{
    multiply <- floor(len / trackDistance)
  }
  
  # calculate and assign  number of tracklines
  tracks <- floor(crosslen / crossDistance)
  
  #set initial heading
  heading <- updir
 
  
  # set universal view direction of the uav
  if (abs(as.numeric(flightParams["uavViewDir"])) == 0) {
    uavViewDir <- updir
  }
  else {
    uavViewDir <- abs(as.numeric(flightParams["uavViewDir"]))
  }
  
  # init of control id #1 common  #99 turnpoints of single tracks
  group <- 1
  
  # set cumulative flightlength to zero
  flightLength <- 0
  
  # initialize djiDF and mav
  djiDF <- data.frame()
  mavDF <- data.frame()
  
  # define output line var
  lns <- list()
  
  lns <- launch2flightalt(p, lns, uavViewDir, launch2startHeading, uavType)
  
  
  # assign starting point
  pos <- c(p$lon1, p$lat1)
  
  footprint <- calcCamFoot(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0,factor)
  footprint<-  sp::spTransform(footprint,sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  landscape<-abs(abs(footprint@bbox[1]-footprint@bbox[3])*overlap-abs(footprint@bbox[1]-footprint@bbox[3]))
  portrait<- abs(abs(footprint@bbox[2]-footprint@bbox[4])*overlap-abs(footprint@bbox[2]-footprint@bbox[4]))
  
  
  # calculates the footprint of the first position and returns a SpatialPolygonsDataFrame
  if (picFootprint)  camera <- calcCamFoot(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0,factor)
  else  camera = "NULL"
  
  ## creates the export control parameter set of the first position
  ##
  if (uavType == "dji_csv") {
    lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
  }
  if (uavType == "pixhawk") {
    lns[length(lns) + 1] <-  makeUavPointMAV(lat = pos[2],lon = pos[1], head = uavViewDir, group = 99 )
  }
  # push pos to old pos
  pOld <- pos
  
  ## set counter and params for mode = "track" mode
  if (mode == "track") {
    if (uavType == "dji_csv") {
      lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
    }
    if (uavType == "pixhawk") {
      lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
    }
    #trackDistance <- len
    multiply <- 1
  }
  
  ## set counter and params for mode = "waypoints"
  else if (mode == "waypoints") {
    if (uavType == "dji_csv") {
      lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
    }
    if (uavType == "pixhawk") {
      lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
    }
  }
  
  ## set counter and params for mode = "terrainTrack"
  else if (mode == "terrainTrack") group = 99
  
  
  #########################################
  #########################
  
  ## now start calculating the waypoints according to the resolution
  cat("calculating waypoints...\n")
  pb <- pb <- utils::txtProgressBar(max = tracks, style = 3)
  # then do for the rest  forward and backward
  for (j in seq(1:tracks)) {
    for (i in seq(1:multiply)) {
      if (mode == "waypoints" || mode == "terrainTrack") {
        if (i >= multiply) {
          group <- 99
        }
        else      {
          group <- 1
        }
      }
      else {
        i <- 2
      }
      
      # calc next coordinate
      pos <- calcNextPos(pOld[1], pOld[2], heading, trackDistance)
      if (picFootprint) camera <- maptools::spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir, trackDistance, flightAltitude,i,j,factor))
      pOld <- pos
      flightLength <- flightLength + trackDistance
      if (mode == "track") {
        group <- 99
      }
      if (uavType == "dji_csv") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = group, p)
      }
      if (uavType == "pixhawk") {
        lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2], lon = pos[1], head = uavViewDir, group = group)
      }
    }
    
    if ((j %% 2 != 0)) {
      pos <- calcNextPos(pOld[1], pOld[2], crossdir, crossDistance)
      if (picFootprint) camera <-  maptools::spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir, trackDistance,flightAltitude,i,j,factor))
      pOld <- pos
      flightLength <- flightLength + crossDistance
      if (uavType == "dji_csv") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      }
      if (uavType == "pixhawk") {
        lns[length(lns) + 1] <-
          makeUavPointMAV(
            lat = pos[2],
            lon = pos[1],
            head = uavViewDir,
            group = 99
          )
      }
      heading <- downdir
    }
    
    else if ((j %% 2 == 0)) {
      pos <- calcNextPos(pOld[1], pOld[2], crossdir, crossDistance)
      if (picFootprint) camera <- maptools::spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir,trackDistance,flightAltitude,i,j,factor))
      pOld <- pos
      flightLength <- flightLength + crossDistance
      
      if (uavType == "dji_csv") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
        heading <- updir
      }
      if (uavType == "pixhawk") {
        lns[length(lns) + 1] <-  makeUavPointMAV( lat = pos[2], lon = pos[1], head = uavViewDir - 180, group = 99)
        heading <- updir
      }
      
    }
    # status bar
    utils::setTxtProgressBar(pb, j)
  }
  close(pb)
  }
  
  ##########################
  ##########################################
  
  #estimate time regarding parameter
  ft <- calculateFlightTime( maxFlightTime,
                             windCondition,
                             maxSpeed,
                             uavOptimumSpeed,
                             flightLength,
                             totalTrackdistance,
                             picRate,
                             logger)
  
  rawTime <- ft[1]
  maxFlightTime <- ft[2]
  maxSpeed <- ft[3]
  picIntervall <- ft[4]
  
  
  
  
  # postprocessing
  fileConn <- file(file.path(runDir,"tmp.csv"))
  cat("preprocessing DEM related stuff...\n")
  if (uavType == "dji_csv") {
    #browser()
    # dump lns to file for read in as csv
    writeLines(unlist(lns[1:length(lns) - 1]), fileConn)
    djiDF <- utils::read.csv(file.path(runDir,"tmp.csv"), sep = ",", header = FALSE)
    # add correct header
    names(djiDF) <-unlist(strsplit(makeUavPoint(pos,uavViewDir,group =group,p,header = TRUE,sep = ','),split = ","))
    # make it spatial
    sp::coordinates(djiDF) <- ~ lon + lat
    sp::proj4string(djiDF) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    # now DEM stuff

    result <- analyzeDSM(demFn,djiDF,p,altFilter,horizonFilter,followSurface,followSurfaceRes,logger,projectDir,dA,dateString,locationName,runDir,taskarea=taskArea,gdalLink)
    # assign adapted dem to demFn
    demFn <- result[[3]]
    dfcor <- result[[2]]
    
    # max numbers of dji waypoints is due to factory limits 98
    # according to start and rth safety we need 6 points for organizig the splitted task
    nofiles <- ceiling(nrow(dfcor@data) / 90)
    maxPoints <- 90
    minPoints <- 1
    # check if the flighttime is forcing more files
    if (nofiles < ceiling(rawTime / maxFlightTime)) {
      nofiles <- ceiling(rawTime / maxFlightTime)
      maxPoints <- ceiling(nrow(dfcor@data) / nofiles) + 1
      mp <- maxPoints
      minPoints <- 1
    }
    # start the creation of the control file(s)
    cat('generate control files...\n')

    # generate single tasks waypoint file for DJI Litchi import format
    calcDjiTask( result[[2]],taskName,nofiles,maxPoints,p,logger, round(result[[6]], digits = 0), trackSwitch=FALSE,file.path(runDir,"tmpdem.tif"),result[[8]], projectDir,dateString,locationName,runDir)
  }
  else if (uavType == "pixhawk") {
    writeLines(unlist(lns), fileConn)
    mavDF <- utils::read.csv(file.path(runDir,"tmp.csv"), colClasses=c("V4"="character",
                                                                       "V5"="character",
                                                                       "V6"="character",
                                                                       "V7"="character"),sep = "\t", header = FALSE)
    names(mavDF) <- c("a","b","c","d","e","f","g","latitude","longitude","altitude","id","j","lat","lon")
    sp::coordinates(mavDF) <- ~ lon + lat
    sp::proj4string(mavDF) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    
    if (is.null(launchAltitude)) {
      # analyze DEM related stuff

      result <- analyzeDSM(demFn,mavDF,p,altFilter,horizonFilter ,followSurface,followSurfaceRes,logger,projectDir,dA,dateString,locationName,runDir,taskArea,gdalLink)
      # assign adapted dem to demFn
      lauchPos <- result[[1]]
      dfcor <- result[[2]]
      demFn <- result[[3]]
      
      nofiles <- ceiling(rawTime / maxFlightTime)
      maxPoints <- ceiling(nrow(dfcor@data) / nofiles) + 1
      
    }
    # generate single tasks waypoint file for MAV pixhawk format
    calcMAVTask(result[[2]],
                taskName,
                nofiles,
                rawTime,
                mode,
                trackDistance,
                maxFlightTime,
                logger,
                p,
                len, 
                multiply,
                tracks,
                result,
                maxSpeed / 3.6,
                uavType,
                file.path(runDir,"flightDEM.tif"),
                maxAlt = result[[6]], 
                projectDir,
                dateString,
                locationName,
                uavViewDir,
                cmd,
                runDir)
  }
  close(fileConn)
  
  # if heatMap is requested
  if (heatMap) {
    cat("calculating picture coverage heat map\n")
    fovH <- calcFovHeatmap(camera, result[[4]])
  } else
  {
    fovH <- "NULL"
  }
  
  # call rcShed
  ##if (!is.null(rcRange)) {
  
  ##  cat("calculating RC-range\n")
  ##  rcCover <-
  ##    rcShed(
  ##      launchP = c(as.numeric(p$launchLon), as.numeric(p$launchLat)),
  ##      flightAlt =  as.numeric(p$flightAltitude),
  ##      rcRange = rcRange,
  ##      dem = result[[4]]
  ##    )
  ##} else {
  rcCover = "NULL"
  ##}
  
  
  
  
  # write log file status and params
  log4r::levellog(logger, 'INFO', paste("taskName     : ", taskName))
  log4r::levellog(logger, 'INFO', paste("DEM filename    : ", names(demFn)))
  log4r::levellog(logger, 'INFO', paste("surveyArea      : ", surveyAreaUTM))
  log4r::levellog(logger, 'INFO', paste("launchAltitude  : ", launchAltitude))
  log4r::levellog(logger, 'INFO', paste("followSurface   : ", followSurface))
  log4r::levellog(logger, 'INFO', paste("altfilter       : ", altFilter))
  log4r::levellog(logger, 'INFO', paste("horizonFilter   : ", horizonFilter))
  log4r::levellog(logger, 'INFO', paste("flightPlanMode  : ", flightPlanMode))
  log4r::levellog(logger, 'INFO', paste("flightAltitude  : ", flightAltitude))
  log4r::levellog(logger, 'INFO', paste("presetFlightTask: ", presetFlightTask))
  log4r::levellog(logger, 'INFO', paste("curvesize       : ", p$curvesize))
  if (uavType == "dji_csv"){
    log4r::levellog(logger, 'INFO', paste("rotationdir     : ", p$rotationdir))
    log4r::levellog(logger, 'INFO', paste("gimbalmode      : ", p$gimbalmode))
    log4r::levellog(logger, 'INFO',paste("gimbalpitchangle: ", p$gimbalpitchangle))
  }
  log4r::levellog(logger, 'INFO', paste("overlap         : ", overlap))
  log4r::levellog(logger, 'INFO', paste("uavViewDir      : ", uavViewDir))
  log4r::levellog(logger, 'INFO', paste("picFootprint    : ", picFootprint))
  log4r::levellog(logger,'INFO',paste("followSurfaceRes: ", followSurfaceRes))
  log4r::levellog(logger, 'INFO', paste("surveyAreaCoords: ", list(surveyArea)))
  log4r::levellog(logger, 'INFO', paste("windCondition   : ", windCondition))
  log4r::levellog(logger,'INFO',paste("calculated mission time    : ", rawTime,      "  (min)      "))
  log4r::levellog(logger,'INFO',paste("estimated battery lifetime  : ", maxFlightTime,      "  (min)      "))
  log4r::levellog(logger,'INFO',paste("Area covered               : ", surveyAreaUTM / 10000,      "  (ha)"))
  log4r::levellog(logger, 'INFO', "-")
  log4r::levellog(logger,'INFO',"----- use the following task params! --------------")
  log4r::levellog(logger,'INFO',paste("RTH flight altitude: ", round(result[[6]], digits = 0), " (m)"))
  log4r::levellog(logger,'INFO',paste("max flight speed   : ",round(maxSpeed, digits = 1),"  (km/h)      "))
  log4r::levellog(logger,'INFO',paste("picture lapse rate : ", picIntervall, "  (sec/pic) "))
  log4r::levellog(logger, 'INFO', paste("trigger distance portrait     : ", portrait))
  log4r::levellog(logger, 'INFO', paste("trigger distance landscape     : ", landscape))
  log4r::levellog(logger,'INFO',"--------------------- END RUN -----------------------------")
  
  # return params for visualisation and main results for overview
  if ((flightPlanMode == 'track' | flightPlanMode == 'terrainTrack') & rawTime > maxFlightTime)  {
    note <- "flighttime > battery lifetime! control files have been splitted. \n Fly save and have Fun..."
  }
  else if (flightPlanMode == 'waypoints') {
    note <- "control files are splitted after max 98 waypoints (litchi control file restricted number)"
  }
  else { note <- " Fly save and have Fun..." }
  dumpFile(paste0(file.path(projectDir, locationName, dateString, "fp-data/log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'))
  cat("\n ",
      "\n NOTE 1:",as.character(note),"",
      "\n NOTE 2: You will find all parameters in the logfile:",paste0(file.path(projectDir, locationName, dateString, "fp-data/log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'),"","\n ")
  x <- c(result[[1]], # launch Pos
         result[[2]], # waypoints
         result[[5]], # resampled dem contour
         result[[3]], # original DEM
         result[[4]], # resampled dem
         camera,      # camera footprint (DJI only)
         taskArea,    # Area of flight task
         rcCover,     # Estimated area that is covered by RC
         fovH)        # Heatmap of overlapping Pictures
  names(x) <- c("lp", "wp", "demA", "oDEM", "rDEM", "fp", "fA", "rcA", "hm")
  system(paste0("rm -rf ",file.path(projectDir,locationName,dateString,"fp-data/run")))
  return(x)
}
