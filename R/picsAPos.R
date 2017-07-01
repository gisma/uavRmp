if (!isGeneric('picsAtPos')) {
  setGeneric('picsAtPos', function(x, ...)
    standardGeneric('picsAtPos'))
}
#' calculates a obstacle free flight path for a defined list of target positions
#' @description  picsAtPos generates a flight track chaining up point objects with respect to a heterogenous Surface and known obstacles as documented by an DSM for taking top down pictures. 
#' @param projectDir path to the main folder where several projects can be hosted It will overwrite the DEM based estimation if any other value than -9999
#' @param demFn  filename of the used DSM data file
#' @param locationName base name string of the mission
#' @param presetFlightTask (DJI only experimental) strongly recommended to use "remote" \cr
#'  Options are: \cr
#' \code{"simple_ortho"} takes one picture/waypoint,
#' \code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an angele of -60deg,
#' \code{"simple_pano"} takes a 360 deg panorama picture and 
#' \code{"remote"} which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude set the default flight altitude (note AGL while the provided raster model represents this surface) of the mission. You have to defined the position of launching.
#' By default it is set to (\code{= 0.0}). If set to \code{-99} it will be 
#' calculated from the swath width of the pictures. NOTE This makes only sense for 
#' \code{followingTerrain = TRUE} to smooth curves.
#' For \code{flightPlanMode = "waypoint"} camera actions are DISABLED during curve flights.
#' @param maxSpeed  cruising speed
#' @param windCondition 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h
#' @param uavType type of uav. currently "djip3" and "solo" are supported
#' @param missionTrackList mission tracklist (target positions)
#' @param launchPos launch position c(lon,lat), c(8.772055,50.814689)
#' @param climbDist distance within the uav will climb on the caluclated save flight altitude in meter
#' @param aboveTreeAlt minum height above target trees in meter
#' @param circleRadius radius to circle around above target trees in meter
#' @param takeOffAlt altitude (MSL) of take off position in meter
#' @param altFilter allowed altitude differences in meter
#' @param launchAltitude altitude of launch position. If set to -9999 a DEM is required for extracting the MSL. Default is -9999
#' @param followSurfaceRes followSurfaceRes
#' @param maxFL maxFL
#' @examples
#'\dontrun{
#' requires(mapview)
#' targetPosPics  <-  picsAtPos(projectDir ="/home/creu/uav/bayerwald",
#'                            locationName = "filzmoosTree",
#'                            missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
#'                            demFn = "~/uav/grossfilz/grosserfilz.tif",
#'                            windCondition = 2,
#'                            uavType = "solo",
#'                            followSurfaceRes=5,
#'                            launchPos = c(8.772055,50.814689))
#' # view result
#' mapview(targetPosPics$wp,zcol = "altitude",lwd=1,cex=5)+
#' mapview(t3$lp,color="red",cex=5)
#' }
#' 
#' @export picsAtPos 
#'               

picsAtPos <-  function(projectDir="~",
                     locationName="autoflightcontrol",
                     missionTrackList=NULL,
                     launchPos=NULL,
                     demFn=NULL,
                     flightAltitude=75,
                     climbDist=7.5,
                     aboveTreeAlt=15,
                     circleRadius = 5.0,
                     takeOffAlt = 50.0,
                     presetFlightTask="remote",
                     maxSpeed=25.0,
                     followSurfaceRes=5,
                     altFilter=1.0,
                     maxFL=10,               
                     windCondition=1,
                     launchAltitude=-9999,
                     uavType="solo") {
  # due to RMD Check Note
  task <- NULL
  demFn  <-  path.expand(demFn)
  # assign flight mission name 
  mission <- paste(locationName, sep=.Platform$file.sep)
  
  workingDir <- locationName
  # create directories if needed
  if(!file.exists(file.path(projectDir, workingDir))){dir.create(file.path(projectDir, workingDir),recursive = TRUE)}
  if(!file.exists(file.path(projectDir, workingDir,"tmp"))){  dir.create(file.path(projectDir, workingDir,"/tmp"),recursive = TRUE)}
  if(!file.exists(file.path(projectDir, workingDir,"run"))){  dir.create(file.path(projectDir, workingDir,"/run"),recursive = TRUE)}
  if(!file.exists(file.path(projectDir, workingDir,"control"))) { dir.create(file.path(projectDir, workingDir,"control"),recursive = TRUE)}
  if(!file.exists(file.path(projectDir,"data"))){dir.create(file.path(projectDir,"data"),recursive = TRUE)}
  # setting R environ temp folder to the current working directory
  Sys.setenv(TMPDIR=file.path(projectDir, workingDir,"run"))
  
  # set R working directory
  setwd(file.path(projectDir, workingDir,"run"))
  
  Sys.chmod(list.dirs("../.."), "777")
  
  # create log file
  logger  <-  log4r::create.logger(logfile = paste0(file.path(projectDir, workingDir,"control/"),strsplit(basename(mission), "\\.")[[1]][1],'.log'))
  log4r::level(logger)  <-  "INFO"
  log4r::levellog(logger, 'INFO',"                                                           ")
  log4r::levellog(logger, 'INFO',"                                                           ")
  log4r::levellog(logger, 'INFO',"--------------------- START RUN ---------------------------")
  log4r::levellog(logger, 'INFO',paste("Working folder: ",file.path(projectDir, workingDir)))
  
  
  
  # create misson filename
  csvFn <-  paste(file.path(projectDir, workingDir,"control"), paste0(mission,".csv"), sep=.Platform$file.sep)
  
  # import flight area if provided by an external vector file
  #file.copy(overwrite = TRUE, from = missionTrackList, to = file.path(projectDir,"data"))
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
  p$maxFL=maxFL
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
  
  fullTreeList <- makeFlightPathT3(flightList,p,uavType,task,demFn,logger,projectDir,locationName,circleRadius,flightArea)
}
