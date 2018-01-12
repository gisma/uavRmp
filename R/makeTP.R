if (!isGeneric('makeTP')) {
  setGeneric('makeTP', function(x, ...)
    standardGeneric('makeTP'))
}
#' Flight Track Planning tool 
#' @description  makeTP generates a flight track chaining up point objects with respect to a heterogenous surface and known obstacles as documented by an DSM for taking top down pictures. It creates a single control file for autonomous picture retrieval flights. 
#' @param projectDir \code{character} path to the main folder where several projects can be hosted, default is \code{tempdir()}
#' @param demFn  \code{character} filename of the used DSM data file, default is \code{NULL}
#' @param locationName \code{character} base name string of the mission, default is \code{"treePos"}
#' @param presetFlightTask \code{character} (DJI only EXPERIMENTAL). NOTE: it is strongly recommended to use the default \code{"remote"} \cr
#'  Further options are: \cr
#' \code{"simple_ortho"} takes one picture/waypoint,
#' \code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an angele of -60deg,
#' \code{"simple_pano"} takes a 360 deg panorama picture and 
#' \code{"remote"} which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude \code{numeric} set the AGL flight altitude (AGL while the provided raster model represents this surface) of the mission, default is \code{100}
#' default is (\code{= 0.0}). If set to \code{-99} it will be 
#' calculated from the swath width of the pictures. NOTE: This makes only sense for 
#' \code{followingTerrain = TRUE} to smooth curves.
#' For \code{flightPlanMode = "waypoint"} camera actions (DJI only EXPERIMENTAL) are DISABLED during curve flights.
#' @param maxSpeed \code{numeric}  cruising speed, default is \code{25.0}
#' @param windCondition \code{numeric}options are 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h, default is \code{1}
#' @param uavType \code{character}  type of UAV. Currently "djip3" and "solo" are supported, default is \code{"solo"}
#' @param missionTrackList \code{character} filename of the mission tracklist (target positions), default is \code{NULL}
#' @param launchPos \code{list} launch position c(longitude,latitude), default is \code{c(8.772055,50.814689)}
#' @param climbDist \code{numeric} distance within the uav will climb on the caluclated save flight altitude in meter, default is \code{7.5}
#' @param aboveTreeAlt \code{numeric} minimum flight height above target trees in meter, default is \code{15.0}
#' @param circleRadius \code{numeric} radius to circle around above target trees in meter, default is \code{1.0}
#' @param takeOffAlt altitude \code{numeric} climb altitude of the uav at take off position in meter, default is \code{50.0}
#' @param altFilter \code{numeric} allowed altitude differences bewteen two waypoints in meter, default is \code{0.5}
#' @param launchAltitude \code{numeric} altitude of launch position. If set to \code{-9999} a DEM is required for extracting the MSL, default is \code{-9999}
#' @param followSurfaceRes \code{numeric}, default is \code{5} meter.
#' @param cameraType \code{character}, default is \code{"MAPIR2"}.
#' @param copy \code{boolean} copy used file to data folder default is \code{FALSE}
#' @param runDir \code{character} runtime folder 

#' @examples
#'\dontrun{
#' ## (1) get example DEM data
#' dsmFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#' ## (2) make position flight plan
#' makeTP  <-  makeTP(missionTrackList= tutorial_flightArea,
#'                   demFn = dsmFn,
#'                   uavType = "solo",
#'                   launchPos = c(8.679,50.856))
#' }
#' @export makeTP 
#'               

makeTP <-  function(projectDir=tempdir(),
                    locationName="treePos",
                    missionTrackList=NULL,
                    launchPos=c(8.772055,50.814689),
                    demFn=NULL,
                    flightAltitude=100,
                    climbDist=7.5,
                    aboveTreeAlt=15,
                    circleRadius = 1.0,
                    takeOffAlt = 50.0,
                    presetFlightTask="remote",
                    maxSpeed=25.0,
                    followSurfaceRes=5,
                    altFilter=0.5,
                    windCondition=1,
                    launchAltitude=-9999,
                    uavType="solo",
                    cameraType = "MAPIR2",
                    copy = FALSE,
                    runDir="") {
  # due to RMD Check Note
  task <- NULL
  demFn  <-  path.expand(demFn)
  locationName <- paste0(locationName,"_missions")
  surveyArea <- missionTrackList
  projstru <- setProjStructure (projectDir,
                                locationName, 
                                
                                flightAltitude,
                                uavType,
                                cameraType,
                                surveyArea,
                                demFn,
                                copy,
                                "P")
  dateString <- projstru[3]
  taskName <- projstru[2]
  csvFn <- projstru[1]
  projectDir<-projstru[4]
  
  runDir<-makeGlobalVar(name = "runDir",value = file.path(projectDir,"fp-data/run/"))
 
  logger <- log4r::create.logger(logfile = paste0(file.path(projectDir, "fp-data/log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'))
  log4r::level(logger) <- "INFO"
  log4r::levellog(logger,'INFO',"--------------------- START RUNfile.path(runDir, ---------------------------")
  log4r::levellog(logger, 'INFO', paste("Working folder: ", file.path(projectDir)))
  # 
  # 
  # # create misson filename
  flightList <- readTreeTrack(missionTrackList)
  test <- try(readLaunchPos(launchPos))
  if (class(test)!="try-error"){
    launchPos <- test
    flightArea <-  flightList+launchPos
  }
  else{
    log4r::levellog(logger, 'FATAL', "### can not find/read launchPosition")        
    stop("### could not read launchPosition")
  }
  #
  p <- list()
  p$launchLat <- launchPos@coords[2]
  p$launchLon <- launchPos@coords[1]
  p$locationName <- locationName
  p$missionTrackList <- missionTrackList
  p$demFn <- demFn
  p$flightAltitude <- flightAltitude
  p$presetFlightTask <- presetFlightTask
  p$maxSpeed <- maxSpeed
  p$followSurfaceRes <- followSurfaceRes
  p$windCondition <- windCondition
  p$uavType <- uavType
  p$curvesize <- 0
  p$rotationdir <- 0
  p$gimbalmode <- 0
  p$gimbalpitchangle <- -90
  p$launchAltitude <- launchAltitude
  p$aboveTreeAlt <- aboveTreeAlt
  p$altFilter <- altFilter
  p$projectDir <- projectDir
  p$climbDist <- climbDist
  p$task <-  fp_getPresetTask("treetop")
  
  fullTreeList <- makeFlightPathT3(flightList,
                                   p,
                                   uavType,
                                   task,
                                   demFn,
                                   logger,
                                   projectDir,
                                   locationName,
                                   circleRadius,
                                   flightArea,
                                   takeOffAlt,
                                   runDir)
  
  # write log file status and params
  log4r::levellog(logger, 'INFO', paste("taskName     : ", taskName))
  log4r::levellog(logger, 'INFO', paste("DEM filename    : ", demFn))
  log4r::levellog(logger, 'INFO', paste("launchAltitude  : ", launchAltitude))
  log4r::levellog(logger, 'INFO', paste("followSurface   : ", followSurfaceRes))
  log4r::levellog(logger, 'INFO', paste("altfilter       : ", altFilter))
  log4r::levellog(logger, 'INFO', paste("flightAltitude  : ", flightAltitude))
  log4r::levellog(logger, 'INFO', paste("flightAltitude  : ", aboveTreeAlt))  
  log4r::levellog(logger, 'INFO', paste("flightAltitude  : ", circleRadius))
  log4r::levellog(logger, 'INFO', paste("flightAltitude  : ", takeOffAlt))    
  log4r::levellog(logger, 'INFO', paste("presetFlightTask: ", presetFlightTask))
  
  if (uavType == "djiP3"){
    log4r::levellog(logger, 'INFO', paste("curvesize       : ", p$curvesize))
    log4r::levellog(logger, 'INFO', paste("rotationdir     : ", p$rotationdir))
    log4r::levellog(logger, 'INFO', paste("gimbalmode      : ", p$gimbalmode))
    log4r::levellog(logger, 'INFO',paste("gimbalpitchangle: ", p$gimbalpitchangle))
  }
  
  log4r::levellog(logger,'INFO',paste("max flight speed   : ",round(maxSpeed, digits = 1),"  (km/h)      "))
  log4r::levellog(logger,'INFO',"--------------------- END RUN -----------------------------")
  
  note <- " Fly save and have Fun..." 
  dumpFile(paste0(file.path(projectDir,  "fp-data/log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'))
  cat("\n NOTE: You will find all parameters in the logfile:\n",paste0(file.path(projectDir, "fp-data/log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'),"","\n ",
      "\n Fly save and have Fun...")
  
}
