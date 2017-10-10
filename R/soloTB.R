
if (!isGeneric('solo_upload')) {
  setGeneric('solo_upload', function(x, ...)
    standardGeneric('solo_upload'))
}
#' upload mission file to solo
#'
#' @description  solo_upload provides a crude interface to upload the Solo mission file to the 3dr SOLO
#'
#' @param connection a valid connection string to the Solo default is "udp:10.1.1.166:14550"
#' @param prearm controls the prearm status of the Solo prearm check 0=Disabled,1=Enabled,-3=Skip Baro,-5=Skip Compass,-9=Skip GPS,-17=Skip INS,-33=Skip Params/Rangefinder,-65=Skip RC,127=Skip Voltage 
#' @param missionFile mission file to upload
#' 
#'
#'
#'
#'@note for using the solo stuff you need to install: \cr sudo pip install pymavlink;\cr sudo pip install dronekit-sitl;\cr sudo pip install dronekit; \cr sudo apt-get install sshpass\cr Additionally you need to be connected to a running 3DR Solo uav 
#'
#' @examples
#' wp <- system.file("extdata", "MAVLINK_waypoints.txt", package = "uavRmp")
#' \dontrun{
#' solo_upload( missionFile = wp)
#' }
#' @export solo_upload
#'               

solo_upload <- function(missionFile = NULL, 
                        connection = "udp:10.1.1.166:14550", 
                        prearm = "-9"){
  
  command <- 'python'
  script <- paste(system.file(package="uavRmp"), "python/io_solo_mission.py", sep = "/")
  option1 <- '--connect'
  connection <- connection
  option2 <- '--prearm'
  prearm <- prearm
  option3 <- '--mission'
  missionFile <- missionFile
  args = c(option1, connection,option2,prearm,option3,missionFile)
  
  # Add path to script as first arg
  allArgs = c(script, args)
  
  output = system2(command, 
                   args = allArgs, 
                   stdout = TRUE)
  option1<-paste0(logDir,"/",logFiles)
  print(paste("Solo returns:", output,"\n"))
}



if (!isGeneric('soloLog')) {
  setGeneric('soloLog', function(x, ...)
    standardGeneric('soloLog'))
}
#' Download, reorganize and export the log files from 3DR Solo and radio control unit
#'
#' @description  Wraps the mavtogpx.py converter as provided by the dronkit library. It downloads and/ or converts the 3DR Solo logfiles. Otionally you may import the geometries and data as sp objects in R
#'
#' @param logSource \code{rc} = logfiles from the radio control, \code{pixhawk} = logfiles from the flightcontroller, default is \code{rc}. The radio control is providing the last ten telemetry data files, while the flight controller provides the latest 50 binary logfiles.

#' @param logFiles If logSource = "pixhawk" the argument "recent" will download the most recent logfile from the pixhawk, other options are "all" for all logfiles or a plain number i.e. "1" for a specific one.
#' @param logDir (existing) destination path to which the logs should be downloaded to 
#' @param downloadOnly default = TRUE, set to FALSE  if you want to convert the log files from the solo remote controller to GPX files
#' @param netWarn if true warns and waits before starting a connection to the controller to connect to the solo wifi
#' @param organize renames the log and gpx files according to their timeperiod
#' 
#' @param makeSP generates SP objects from the gpx files
#' 
#' @note for using the solo stuff you need to install: \cr 
#' sudo pip install pymavlink  \cr 
#' sudo pip install dronekit-sitl \cr 
#' sudo pip install dronekit \cr 
#' sudo apt-get install sshpass \cr 
#' \cr Additionally you need to be connected to a running 3DR Solo uav 
#' 
#' @examples
#' \dontrun{
#' ## download recent telemetry log file from controller and convert it to gpx
#' soloLog(logFiles = "solo.tlog")
#' 
#' ## download the last available logfile from the radio control
#' soloLog()
#' 
#' ## download ALL logfiles from the radio control
#' soloLog(logFiles = "all")
#' 
#' ## download ALL telemetry logfiles from the flight controller
#' soloLog(logSource = "pixhawk",logFiles = "all")
#' 
#' ## download telementry logfile number 5  from the remote control
#' soloLog(logSource = "rc",logFiles = "5")

#' }
#' @export soloLog
#'               

soloLog <- function(logFiles="recent",
                    logSource="rc",
                    logDir=tmpDir(), 
                    downloadOnly=FALSE,
                    netWarn=TRUE,
                    organize=TRUE,
                    makeSP = FALSE){
  exit<-NULL
  logDir<- path.expand(logDir)
  command <-"mavtogpx.py"
 # option1<-paste0(logDir,"/",logFiles)
  
  if (!file.exists(file.path(logDir))) {
    dir.create(file.path(logDir), recursive = TRUE)
  }
  if (logSource=="pixhawk") downloadOnly=TRUE
  
  invisible(readline(prompt="Press [enter] to continue\n The controller shutdown after a while - check connection\n"))
  cat("downloading and converting will take a while without prompting anything...\n be patient in the end you will know.\n")
  
  if (logSource == "rc")  {
    if (logFiles=="all") {logFiles<-"solo.t*"}
    else if (logFiles=="recent") {logFiles<-"solo.tlog"}
    else  logFiles<-paste("solo.tlog.",logFiles)
    option1<-paste0(logDir,"/",logFiles)  
      cat("you will download the following files:\n")
      cat(system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.1 ls -l -h /log/",logFiles),intern = TRUE))
    
    log<-system( paste0("sshpass -p 'TjSDBkAu'  scp 'root@10.1.1.1:/log/",logFiles,"' ",logDir,"/. " ),wait = TRUE)
  } else {
    loglist <- system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 ls -h /log/dataflash"),intern = TRUE)
    lastlog<-system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 cat /log/dataflash/LASTLOG.TXT"),intern = TRUE)
    if (logFiles == "all"){
      cat("you will download the following files:\n")
      cat(system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 ls -l -h /log/dataflash"),intern = TRUE))
      log<-system( paste0("sshpass -p 'TjSDBkAu'  scp -P 22 root@10.1.1.10:/log/dataflash/*.BIN ",logDir,"/. " ),wait=TRUE)
    } else if (logFiles == "recent"){
      cat("you will download the following files:\n")
      cat(system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 ls -l -h /log/dataflash/",unlist(strsplit(lastlog, "\r")[[1]]),".BIN"),intern = TRUE))
      log<-system( paste0("sshpass -p 'TjSDBkAu'  scp -P 22 root@10.1.1.10:/log/dataflash/",unlist(strsplit(lastlog, "\r")[[1]]),".BIN ",logDir,"/. " ),wait=TRUE)
    } else {
      log<-system( paste0("sshpass -p 'TjSDBkAu'  scp -P 22 root@10.1.1.10:/log/dataflash/",logFiles,".BIN ",logDir,"/. " ),wait=TRUE)
    }
  }

  if (log == 0) {
    f <- list.files(logDir, pattern=extension(logFiles))
    cat(f," downloaded...\n")
    cat("Download from solo controllor seems to be ok\n")
  } else {
    cat('FATAL', "### can not find/read input file")        
    stop("### could not read any log data\n")
  }  
  if (downloadOnly){
    cat("All logs downloaded...")
    return()
  } 
  
  test<-system2("mavtogpx.py","-h",stdout = TRUE)
  if (grep("usage: mavtogpx.py",test)){
    cat("pymavlink seems to be installed\n")
    cat("converting log files to to gpx...\n")
    test<-system2(command, option1,stdout = TRUE)
    cat(test)
  } else {
    stop("No pymavlink lib. Try: sudo pip install pymavlink\n")
  }
  
  
  if (organize) {
    cat("\nrename files...\n")
    f <- list.files(logDir, pattern="gpx")
    
    i=1
    flights <- list()
    for (flight in f) {
      f <- read_gpx(path.expand(paste0(logDir,"/",flight)))
      flights[[i]]<-f
      firstTime<-as.character(flights[[i]]$track_points@data$time)[1]
      lastTime<-as.character(flights[[i]]$track_points@data$time)[length(flights[[i]]$track_points@data$time)]
      la1<-gsub(x = lastTime, pattern = "\\/",replacement = "")
      la2<-gsub(x = la1, pattern = "\\ ",replacement = "_")
      la3<-substr(gsub(x = la2, pattern = "\\:",replacement = "-"),10,17)
      
      fi1<-gsub(x = firstTime, pattern = "\\/",replacement = "")
      fi2<-gsub(x = fi1, pattern = "\\ ",replacement = "_")
      fi3<-substr(gsub(x = fi2, pattern = "\\:",replacement = "-"),1,17)
      logName<- paste0(logDir,"/",fi3,"_",la3,"_solo.tlog")
      gpxName<- paste0(logDir,"/",fi3,"_",la3,"_solo.gpx")      
      fNgpx <- paste0(logDir,"/",list.files(logDir, pattern="gpx"))
      fNlog <- paste0(logDir,"/",list.files(logDir, pattern="tlog",include.dirs = FALSE))
      if (!file.exists(logName)) file.rename(fNlog[i],logName)
      else {cat("you'd already converted ",logName,"\n")}
      if (!file.exists(gpxName)) file.rename(fNgpx[i],gpxName)
      else {cat("you'd already converted ",gpxName,"\n")}
      
      i=i+1
    }
    cat("All logfiles stored and coverted ...\n")
    if (makeSP) {
      cat("export as sp objects ...\n")
      return(flights)}
  }
}
