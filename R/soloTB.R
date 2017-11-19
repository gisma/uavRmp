
if (!isGeneric('solo_upload')) {
  setGeneric('solo_upload', function(x, ...)
    standardGeneric('solo_upload'))
}
#' Upload MAV compliant mission File to a 3DR Solo
#' @description  solo_upload provides a crude interface to upload the Solo mission file to the 3dr SOLO
#'
#' @param connection a valid connection string to the Solo default is "udp:10.1.1.166:14550"
#' @param prearm \code{character} controls the prearm status of the Solo prearm check\cr 0=Disabled\cr 1=Enabled\cr -3=Skip Baro\cr -5=Skip Compass\cr -9=Skip GPS\cr -17=Skip INS\cr -33=Skip Params/Rangefinder\cr -65=Skip RC\cr 127=Skip Voltage\cr
#' default is \code{-1}\cr\cr Find more information at \href{http://ardupilot.org/copter/docs/prearm_safety_check.html}{prearm safety},\cr \href{http://python.dronekit.io/examples/mission_import_export.html}{Mission import export script}.
#' 
#'  
#' @param missionFile mission file to upload
#' 
#'
#'
#'
#'@note Becareful with fooling around with the prearm stuff. It is kind of VERY sensitive for the later autonomous flights!\cr For using the Solo stuff you need to install: \cr sudo pip install pymavlink;\cr sudo pip install dronekit-sitl;\cr sudo pip install dronekit; \cr sudo apt-get install sshpass\cr Additionally you need to be connected to a running 3DR Solo uav 
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
                        prearm = "-1"){
  
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
 # option1<-paste0(logFolder,"/",logFiles)
  print(paste("Solo returns:", output,"\n"))
}



if (!isGeneric('soloLog')) {
  setGeneric('soloLog', function(x, ...)
    standardGeneric('soloLog'))
}
#' Download, reorganize and export the binary log files from 3DR Solo Pixhawk controller or the telemetry log files from the Solo radio control unit
#'
#' @description  Wraps the mavtogpx.py converter as provided by the \href{http://python.dronekit.io/}{dronkit library}). It downloads and optionally converts the most important 3DR Solo logfiles. Optionally you may import the geometries and data as \code{sp} object.
#'
#' @param logSource \code{character}, options are: \code{rc} = logfiles from the radio control, \code{pixhawk} = logfiles from the flightcontroller, default is set to  \code{rc}. The radio control is providing the last ten telemetry data files, while the flight controller provides the latest 50 binary logfiles.

#' @param logFileSample \code{character} , options are:  \code{recent} download the most recent logfile,  \code{all} downloads all logfiles, or a plain number e.g. \code{2} for a specific logfile. Note the telemetry logfiles are numbering from 1 to 9 only, the most recent one is not numbered. The binary logfiles from the pixhawk are numbering continously but only the last 50 files or so will exist. 
#' 
#' @param logDest \code{character} (existing) destination path to which the logs should be downloaded to 
#' @param downloadOnly \code{logical} wether to only download the files or also convert and rename them, default is set FALSE
#' @param netWarn \code{logical} wether to warn and waits before starting a connection to the controller. helps while testing due to occassional wifi shutdowns of the Solo, default is set to FALSE
#' @param renameFiles  \code{logical} renames the log and gpx files according to the time period, default is set TRUE
#' 
#' @param makeSP \code{logical} wether returning an \code{sp} object from the gpx files or not, default is FALSE
#' 
#' @note for using the Solo stuff is tested only for Linux and the bash shell under Windows 10. You need to install the following python libs: \cr 
#' \code{sudo pip install pymavlink}  \cr 
#' \code{sudo pip install dronekit-sitl} \cr 
#' \code{sudo pip install dronekit} \cr \cr
#' Additionally you need \code{sshpass}:\cr
#' \code{sudo apt-get install sshpass} \cr 
#' \cr And please rememeber - you need to be connected at least to a running 3DR Solo radio control and if you want to donload data from the Pixhawk to a Solo UAV
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

soloLog <- function(logFileSample = "recent",
                    logSource     = "rc",
                    logDest       = tempdir(), 
                    downloadOnly  = FALSE,
                    netWarn       = FALSE,
                    renameFiles   = TRUE,
                    makeSP        = FALSE){

  logFolder <- path.expand(logDest)
  command <- "mavtogpx.py"
  # option1<-paste0(logFolder,"/",logFileSample)
  
  if (!file.exists(file.path(logFolder))) {
    dir.create(file.path(logFolder), recursive = TRUE)
  }
  if (logSource=="pixhawk") {
    downloadOnly <- TRUE
    renameFiles <- FALSE
    }
  
  if (netWarn) {
  invisible(readline(prompt="Press [enter] to continue\n The controller shutdown after a while - check connection\n"))
  }
  cat("Downloading and converting will take a while without prompting a lot...\n so be patient!\n")
  
  if (logSource == "rc")  {
    if (logFileSample=="all") {logFiles<-"solo.tlo???"}
    else if (logFileSample=="recent") {logFiles<-"solo.tlog"}
    else  logFiles<-paste("solo.tlog.",logFiles)
    option1<-paste0(logFolder,"/",logFiles)  
    cat("you will download the following files:\n")
    cat(system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.1 ls -l -h /log/",logFiles),intern = TRUE))
    
    log<-system( paste0("sshpass -p 'TjSDBkAu'  scp 'root@10.1.1.1:/log/",logFiles,"' ",logFolder,"/. " ),wait = TRUE)
  } else {
    if (logFileSample=="all") {logFiles<-"*.BIN"}
    else if (logFileSample=="recent") {logFiles<-unlist(strsplit(lastlog, "\r")[[1]])}
    else  logFiles<-paste("solo.tlog.",logFiles)
    loglist <- system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 ls -h /log/dataflash"),intern = TRUE)
    lastlog<-system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 cat /log/dataflash/LASTLOG.TXT"),intern = TRUE)
    if (logFileSample == "all"){
      cat("you will download the following files:\n")
      cat(system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 ls -l -h /log/dataflash"),intern = TRUE))
      log<-system( paste0("sshpass -p 'TjSDBkAu'  scp -P 22 root@10.1.1.10:/log/dataflash/*.BIN ",logFolder,"/. " ),wait=TRUE)
    } else if (logFileSample == "recent"){
      cat("you will download the following files:\n")
      cat(system(paste0("sshpass -p 'TjSDBkAu' ssh  root@10.1.1.10 ls -l -h /log/dataflash/",unlist(strsplit(lastlog, "\r")[[1]]),".BIN"),intern = TRUE))
      log<-system( paste0("sshpass -p 'TjSDBkAu'  scp -P 22 root@10.1.1.10:/log/dataflash/",unlist(strsplit(lastlog, "\r")[[1]]),".BIN ",logFolder,"/. " ),wait=TRUE)
    } else {
      log<-system( paste0("sshpass -p 'TjSDBkAu'  scp -P 22 root@10.1.1.10:/log/dataflash/",logFiles,".BIN ",logFolder,"/. " ),wait=TRUE)
    }
  }
  
  if (log == 0) {
    f <- list.files(logFolder, pattern=extension(logFiles))
    cat(f," downloaded...\n")
    cat("Download from Solo controllor seems to be ok\n")
  } else {
    cat('FATAL', "### can not find/read input file")        
    stop("### could not read any log data\n")
  }  
  
  if (downloadOnly){
    cat("All logs downloaded...")
    return()
  } 

  
  
  if (renameFiles) {
    # create filelists
    tlogFiles <- Sys.glob(file.path(logFolder, "*.tlog.?"))
    tlogFiles <- c(Sys.glob(file.path(logFolder, "*.tlog")),tlogFiles)
    gpxLogFiles <- paste0(tlogFiles,".gpx")  
    test <- system2("mavtogpx.py","-h",stdout = TRUE)
    if (grep("usage: mavtogpx.py",test)) {
      cat("at least pymavlink is installed :-)\n")
      cat("converting log files to to gpx...\n")
      if (logFileSample == "all") {
        for (log in tlogFiles) {
          test <- system2(command, log,stdout = TRUE)
        }
        
      } else  test <- system2(command, option1,stdout = TRUE)
      cat(test)
    } else {
      stop("No pymavlink lib. Try: sudo pip install pymavlink\n")
    }
    
    cat("\nrename files...\n")
    i <- 1
    flights <- list()
    for (flight in gpxLogFiles) {
      f <- read_gpx(flight)
      flights[[i]] <- f
      firstTime <- as.character(flights[[i]]$track_points@data$time)[1]
      lastTime <- as.character(flights[[i]]$track_points@data$time)[length(flights[[i]]$track_points@data$time)]
      la1 <- gsub(x = lastTime, pattern = "\\/",replacement = "")
      la2 <- gsub(x = la1, pattern = "\\ ",replacement = "_")
      la3 <- substr(gsub(x = la2, pattern = "\\:",replacement = "-"),10,17)
      Sys.glob("*.dbf")
      fi1 <- gsub(x = firstTime, pattern = "\\/",replacement = "")
      fi2 <- gsub(x = fi1, pattern = "\\ ",replacement = "_")
      fi3 <- substr(gsub(x = fi2, pattern = "\\:",replacement = "-"),1,17)
      logName <- paste0(logFolder,"/",fi3,"_",la3,"_solo.tlog")
      gpxName <- paste0(logFolder,"/",fi3,"_",la3,"_solo.gpx")      
      #fNgpx <- paste0(logFolder,"/",list.files(logFolder, pattern="solo.tlog.*.gpx"))
      #fNlog <- Sys.glob(file.path(logFolder, "*.tlog.?"))
      #fNlog<-c(Sys.glob(file.path(logFolder, "*.tlog")),fNlog)
      if (!file.exists(logName)) {
        file.rename(tlogFiles[i],logName)
      } else {
        cat("you'd already converted ",logName,"\n")
      }
      if (!file.exists(gpxName)) {
        file.rename(gpxLogFiles[i],gpxName)
      } else {
        cat("you'd already converted ",gpxName,"\n")
      }
      i <- i + 1
    }
    cat("All logfiles stored and coverted ...\n")
    
    if (makeSP) {
      cat("export as sp objects ...\n")
      return(flights)}
  }
}


