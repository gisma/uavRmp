#DEM related preprocessing and basic analysis stuff
# 
# (1)  it imports and deproject different kind of input DEM/DSM data
# (2)  extracting the launching point altitude
# (3)  extracting all altitudes at the waypoints and the "real" agl flight altitude
# (4)  calculating the overall RTH 
# (5)  filtering in line waypoints according to an altitude difference treshold
# (6)  preprocessing of an highest resolution DSM dealing with clearings and other artefacts
# (7)  generates a sp object of the outer boundary of reliable DEM values
#

analyzeDSM <- function(demFn ,df,p,altFilter,horizonFilter,followSurface,followSurfaceRes,logger,projectDir,dA,workingDir,locationName,runDir){
  
  cat("load DEM/DSM data...\n")
  ## load DEM data either from a local GDAL File or from a raster object or if nothing is provided tray to download SRTM data
  #if no DEM is provided try to get SRTM data
  if (is.null(demFn)) {
    log4r::levellog(logger, 'WARN', "CAUTION!!! no DEM file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    stop("\nCAUTION! No DEM data is provided.\n Please download e.g. SRTM data... \n Be aware that the resulution of SRTM is NOT sufficient for terrain following flights!")
  } else {
    
    if (class(demFn)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
      # get information of the raw file
      # project the  extent to the current input ref system 
      proj <- projection(rundem)
      xmn  <- min(p$lon1,p$lon3) - 0.0083
      xmx  <- max(p$lon1,p$lon3) + 0.0083
      ymn  <- min(p$lat1,p$lat3) - 0.0083
      ymx  <- max(p$lat1,p$lat3) + 0.0083
      cut  <- data.frame(y = c(ymn,ymx), x = c(xmn,xmx))
      coordinates(cut) <- ~x+y
      sp::proj4string(cut) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
      cut <- spTransform(cut,CRS(proj))
      rundem <- demFn
      rundem <- raster::crop(rundem,extent(cut@bbox[1],cut@bbox[3],cut@bbox[2],cut@bbox[4]))
      raster::writeRaster(rundem,file.path(runDir,"tmpdem.tif"),overwrite = TRUE)
      demll <- gdalUtils::gdalwarp(srcfile =file.path(runDir, "tmpdem.tif"), dstfile = file.path(runDir,"demll.tif"), 
                        overwrite = TRUE,  
                        t_srs = "+proj=longlat +datum=WGS84 +no_defs",
                        output_Raster = TRUE )  
      file.copy(demFn, paste0(file.path(runDir,"/"),"/tmpdem.tif")) 
      dem  <- demll
      # if GEOTIFF or other gdal type of data
    } else{
      # get information of the raw file
      # project the  extent to the current input ref system 
      tmpproj <- grep(gdalUtils::gdalinfo(path.expand(demFn),proj4 = TRUE),pattern = "+proj=",value = TRUE)
      proj <- substring(tmpproj,2,nchar(tmpproj) - 2)
      xmn  <- min(p$lon1,p$lon3) - 0.007
      xmx  <- max(p$lon1,p$lon3) + 0.007
      ymn  <- min(p$lat1,p$lat3) - 0.007
      ymx  <- max(p$lat1,p$lat3) + 0.007
      cut  <- data.frame(y = c(ymn,ymx), x = c(xmn,xmx))
      coordinates(cut) <- ~x+y
      sp::proj4string(cut) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
      cut <- spTransform(cut,CRS(proj))
      rundem <- raster::raster(demFn,band = 1)
      rundem <- raster::crop(rundem,extent(cut@bbox[1],cut@bbox[3],cut@bbox[2],cut@bbox[4]))
      raster::writeRaster(rundem,file.path(runDir,"tmpdem.tif"),overwrite = TRUE)
      demll <- gdalUtils::gdalwarp(srcfile = file.path(runDir,"tmpdem.tif"), dstfile = file.path(runDir,"demll.tif"), 
                        overwrite = TRUE,  
                        t_srs = "+proj=longlat +datum=WGS84 +no_defs",
                        output_Raster = TRUE )  
      file.copy(demFn, paste0(file.path(runDir,"/"),"/tmpdem.tif")) 
      demll <- setMinMax(demll)
      dem   <- demll
    }
  }  # end of loading DEM data
  
  # check if at least a projection string exist 
  crsString <- comp_ll_proj4((as.character(dem@crs)))
  
  if (!crsString) {
    # stop("the DEM/DSM is not georeferencend - please provide a correct georeferenced raster object or GeoTiff file\n")
    # if so deproject DEM/DSM because all of the vector data is latlong WGS84
    demll <- gdalUtils::gdalwarp(srcfile = file.path(runDir,"tmpdem.tif"), dstfile = file.path(runDir,"demll.tif"), 
                      overwrite = TRUE,  
                      t_srs = "+proj=longlat +datum=WGS84 +no_defs",
                      output_Raster = TRUE )  
    demll <- setMinMax(demll)
  } 
  
  # preprocessing

  
  # extract all waypoint altitudes
  altitude <- raster::extract(demll,df,layer = 1, nl = 1)
  
  # get maximum altitude of the task area
  maxAlt <- max(altitude,na.rm = TRUE)
  
  log4r::levellog(logger, 'INFO', paste("maximum DEM Altitude : ", maxAlt," m"))
  
  # create sp point object from launchpos 
  pos <- as.data.frame(cbind(p$launchLat,p$launchLon))
  sp::coordinates(pos) <- ~V2+V1
  sp::proj4string(pos) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # extract launch altitude from DEM
  if (is.na(p$launchAltitude)) {
    tmpalt <- raster::extract(demll,pos,layer = 1, nl = 1)  
    p$launchAltitude <- as.numeric(tmpalt)
    # otherwise take it from the parameter set
  } else 
  {
    p$launchAltitude <- as.numeric(p$launchAltitude)
  }
  
  log4r::levellog(logger, 'INFO', paste("launching Altitude : ", p$launchAltitude," m"))
  
  # write it back to the p list
  launchAlt <- p$launchAltitude
  flightAltitude <- as.numeric(p$flightAltitude)
  
  # calculate the agl flight altitude shift due to launching and max altitude
  p$flightAltitude <- as.numeric(p$flightAltitude) + (maxAlt - as.numeric(launchAlt))
  
  # make a rough estimation of the overall rth altitude
  rthFlightAlt  <- p$flightAltitude
  p$rthAltitude <- rthFlightAlt
  
  log4r::levellog(logger, 'INFO', paste("rthFlightAlt : ", rthFlightAlt," m"))
  
  # if terrain following filter the waypoints by using the altFilter Value
  if (followSurface) {
    cat("apply follow terrain filter...\n")
    
    # extract all waypoint altitudes
    altitude2 <- raster::extract(demll,df,layer = 1, nl = 1)
    # get maximum altitude of the task area
    
    # extract launch altitude from DEM
    launchAlt <- raster::extract(demll,pos,layer = 1, nl = 1)  
    
    # calculate the agl flight altitude
    #altitude<-altitude+as.numeric(p$flightAltitude)-maxAlt    
    altitude2 <- altitude2 - launchAlt[1] + flightAltitude
    
    
    #write it to the sp object dataframe
    df$altitude <- round(altitude2,1)
    
    # if terraintrack = true try to reduce the number of waypoints by filtering
    # this is done by: 
    # (1) applying the horizonFilter size via the rollmax function of the zoo package
    #     to the raw waypoints altitudes the missing (moving window) values (in the end) are duplicated
    # (2) the resulting values are sampled by the same horizonFilter size distance
    # (3) finally the altFilter is applied
    if ( as.character(p$flightPlanMode) == "terrainTrack") {
      sDF <- as.data.frame(df@data)
      sDF$sortID <- seq(1,nrow(sDF))
      # smooth to maxvalues
      filtAlt       <- data.frame(zoo::rollmax(na.fill(sDF$altitude,"extend"), horizonFilter,fill = "extend"))
      sDF$altitude  <- filtAlt[,1]
      colNames      <- colnames(sDF)
      colnames(sDF) <- colNames
      turnPoints    <- sDF[sDF$id == "99",]
      samplePoints  <- sDF[seq(1, to = nrow(sDF), by = horizonFilter),]
      
      duplicates <- which(!is.na(match(rownames(samplePoints),rownames(turnPoints))))
      fDF <- rbind(turnPoints,samplePoints[-duplicates,])
      fDF <- fDF[order(fDF$sortID),]
      
      dif           <- abs(as.data.frame(diff(as.matrix(fDF$altitude))))
      colnames(dif) <- c("dif")
      fDF           <- fDF[-c(1), ] # drop first line
      fDF$dif       <- dif[,1]
      
      fDF <- fDF[fDF$id == "99" | fDF$dif > altFilter , ]
      
      fDF$lon <- as.numeric(fDF$longitude)
      fDF$lat <- as.numeric(fDF$latitude)
      
      sp::coordinates(fDF) <- ~lon+lat
      sp::proj4string(fDF) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
      fDF@data$sortID      <- NULL
      fDF@data$dif         <- NULL
      
      df <- fDF
    }
  }
  
  # dump flightDEM as it was used for agl prediction
  writeRaster(demll,file.path(runDir,"tmpFlightDEM.tif"),overwrite = TRUE)
  gdalUtils::gdalwarp(srcfile = file.path(runDir,"tmpFlightDEM.tif"), dstfile = file.path(runDir,"tmpdem.tif"),  
           overwrite = TRUE,  
           t_srs = paste0("+proj=utm +zone=",long2UTMzone(p$lon1)," +datum=WGS84"),
           tr = c(as.numeric(p$followSurfaceRes),as.numeric(p$followSurfaceRes))
  )
  if (dA) outputras <- TRUE
  else outputras <- FALSE
  # deproject it again to latlon
  tmpdemll <- gdalUtils::gdalwarp(srcfile = file.path(runDir,"tmpdem.tif"), dstfile = file.path(runDir,"flightDEM.tif"), 
                       t_srs = "+proj=longlat +datum=WGS84 +no_defs",
                       overwrite = TRUE,  
                       output_Raster = outputras 
  )
  
  # create a sp polygon object of the DEM area that is useable for a flight task planning
  if (dA) {
    cat("start demArea analysis - will take a while...\n")
    c        <- clump(tmpdemll > 0)
    demArea  <- rasterToPolygons(c)
    demArea  <- rgeos::gUnaryUnion(demArea)
  } else {
    demArea  <- "NULL"
  }
  
  # return results
  return(c(pos,df,rundem,demll,demArea,rthFlightAlt,launchAlt,maxAlt,p))
}

# export data to MAV xchange format 
# (1) controls with respect to waypoint number and/or battery lifetime the splitting of the mission files to seperate task files
# (2) calculate and insert rth and fts waypoints with respect to the terrain obstacles to generate a save start and end of a task

calcMAVTask <- function(df,mission,nofiles,rawTime,flightPlanMode,trackDistance,batteryTime,logger,p,len,multiply,tracks,param,speed,uavType,dem,maxAlt,projectDir, workingDir,locationName,uavViewDir,cmd,runDir){
  fin <- FALSE
  minPoints <- 1
  # set number of waypoints per file
  maxPoints <- ceiling(nrow(df@data)/nofiles)
  
  if (maxPoints > nrow(df@data)) {maxPoints <- nrow(df@data)}
  
  # set original counter according to battery lifetime or number of points (708)
  addmax <- maxPoints
  cat(paste0("create ",nofiles, " control files...\n"))
  
  # store launch position and coordinates necessary for the rth calculations
  #row1 <- df@data[1,1:(ncol(df@data))]
  launchLat <- df@data[1,8]
  launchLon <- df@data[1,9]
  
  # read dem
  dem <- raster(dem)
  # re-read launch altitude
  launch_pos <- as.data.frame(cbind(launchLat,launchLon))
  sp::coordinates(launch_pos) <- ~launchLon+launchLat
  sp::proj4string(launch_pos) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  launchAlt <- raster::extract(dem,launch_pos,layer = 1, nl = 1)  
  
  # for each splitted task file
  for (i in 1:nofiles) {
    cat(paste0("create ",i, " of ",nofiles, " control files...\n"))  
    
    # for safety issues we need to generate synthetic climb and sink waypoints 
    # take current start position of the split task
    startLat <- df@data[minPoints + 1,8]
    startLon <- df@data[minPoints + 1,9]
    
    # take current end position of split task
    endLat <- df@data[maxPoints,8]
    endLon <- df@data[maxPoints,9]
    
    # depending on DEM/DSM sometimes there are no data values
    if (!is.na(endLat) & !is.na(endLon)) {
      # generate flight lines from launch to start and launch to end point of splitted task
      home  <- sp_line(c(launchLon,endLon),c(launchLat,endLat),"Home",runDir=runDir)
      start <- sp_line(c(launchLon,startLon),c(launchLat,startLat),"Start",runDir=runDir)
      
      # calculate minimum rth altitude for each line by identifying max altitude
      homeRth  <- raster::extract(dem,home, fun = max,na.rm = TRUE,layer = 1, nl = 1) - launchAlt + as.numeric(p$flightAltitude)
      startRth <- raster::extract(dem,start,fun = max,na.rm = TRUE,layer = 1, nl = 1) - launchAlt + as.numeric(p$flightAltitude)
      
      # add 10% of flight altitude as safety buffer
      homeRth  <- homeRth  + 0.1 * homeRth
      startRth <- startRth + 0.1 * startRth
      
      # get the max position of the flight lines
      homemaxpos  <- maxpos_on_line(dem,home)
      startmaxpos <- maxpos_on_line(dem,start)
      
      # calculate heading 
      homeheading  <- geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a = 6378137, f = 1/298.257223563)
      startheading <- geosphere::bearing(c(launchLon,launchLat),c(startLon,startLat), a = 6378137, f = 1/298.257223563)
      
      
      # if maxpoints is greater than the existing number of points reset it
      DF <- df@data[(as.numeric(minPoints) + 1):maxPoints,]
      
      # write and re-read waypoints
      sep <- "\t"
      # keeps <- c("a","b","c","d","e","f","g","latitude","longitude","altitude","j")
      keeps <- c("latitude","longitude","altitude")
      DF <- DF[keeps]
      DF[stats::complete.cases(DF),]
      utils::write.table(DF[,1:(ncol(DF))],file = file.path(runDir,"tmp2.csv"),quote = FALSE,row.names = FALSE,sep = "\t")
      
      #read raw waypoint list
      lns <- data.table::fread(file.path(runDir,"tmp2.csv"), skip = 1L, header = FALSE,sep = "\n", data.table = FALSE)
      
      # define output dataframe
      lnsnew <- data.frame()
      
      # MAV start sequence
      
      # HEADER LINE  
      lnsnew[1,1] <- "QGC WPL 110"
      # HOMEPOINT 
      # lnsnew[2,1] <- mavCmd(id = 0,
      #                       cmd = 179,
      #                       lat = round(p$launchLat,6),
      #                       lon = round(p$launchLon,6),
      #                       alt = round(param$launchAltitude))
      # TAKEOFF
      lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 1, 
                                                 cmd = 22, 
                                                 alt = round(startRth,6))
      # SPEED taxiway
      lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 2, 
                                                 cmd = 178, 
                                                 p2 = round(speed*4.0,6))
      
      # ascent2start WP
      lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 3, 
                                                 cmd = cmd,
                                                 p4 = round(abs(uavViewDir),1),
                                                 lat = round(calcNextPos(launchLon,launchLat,startheading,5)[2],6),
                                                 lon = round(calcNextPos(launchLon,launchLat,startheading,5)[1],6),
                                                 alt = round(startRth,0))
      # maxStartPos WP
      lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 4, 
                                                 cmd = 16,
                                                 p4 = round(abs(uavViewDir),1),
                                                 lat = round(startmaxpos[1,2],6),
                                                 lon = round(startmaxpos[1,1],6),
                                                 alt = round(startRth,0))
      
      # SPEED task
      lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 5, 
                                                 cmd = 178, 
                                                 p2 = round(speed,6))
      lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 6, 
                                                 cmd = 115, 
                                                 p1 = round(abs(uavViewDir),1))
      lc <- 7
      # task WP & task speed
      for (j in  seq(1,(addmax - 1)*2)) {
        if (is.odd(j)){
          sp<- str_split(pattern = "\t",string = lns[ceiling(j/2),])
          lnsnew[j + lc,1] <-   mavCmd(id = j + lc - 1, 
                                       cmd = 16,
                                       p4 = round(abs(uavViewDir),1),
                                       lat = sp[[1]][1],
                                       lon = sp[[1]][2],
                                       alt = sp[[1]][3])}
        else {
          
          lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 6, 
                                                     cmd = 115, 
                                                     p1 = round(abs(uavViewDir),1),
                                                     p2 = 90,
                                                     p4 = 0)
          
          # lnsnew[j + lc,1] <- mavCmd(id = j + lc - 1, 
          #                            cmd = 178, 
          #                            p2 = round(speed,6))
        }
      }
      
      # ascent2home WP
      lnsnew[length(lnsnew[,1]) ,1] <- mavCmd(id = as.character(length(lnsnew[,1]) ), 
                                                 cmd = 16,
                                                 p4 = round(abs(uavViewDir),1),
                                                 lat = round(calcNextPos(endLon,endLat,homeheading,5)[2],6),
                                                 lon = round(calcNextPos(endLon,endLat,homeheading,5)[1],6),
                                                 alt = round(homeRth,0))
      # maxhomepos WP
      lnsnew[length(lnsnew[,1]) ,1] <- mavCmd(id = as.character(length(lnsnew[,1]) ), 
                                                 cmd = 16,
                                                 p4 = round(abs(uavViewDir),1),
                                                 lat = round(homemaxpos[1,2],6),
                                                 lon = round(homemaxpos[1,1],6),
                                                 alt = round(homeRth,0))
      
      # MAV fly to launch sequence
      # RTH altitude TODO
      lnsnew[length(lnsnew[,1]) ,1] <- mavCmd(id = as.character(length(lnsnew[,1]) ), 
                                                 cmd = 30,
                                                 alt = round(homeRth,0))
      # SPEED max return speed
      lnsnew[length(lnsnew[,1]) ,1] <- mavCmd(id = as.character(length(lnsnew[,1]) ), 
                                                 cmd = 178,
                                                 p2 = round(speed*4.0,6))
      # trigger RTL event
      lnsnew[length(lnsnew[,1]) ,1] <- mavCmd(id = as.character(length(lnsnew[,1]) ), 
                                                 cmd = 20)
      
      # write the control file
      utils::write.table(lnsnew, paste0(projectDir, "/",locationName , "/", workingDir,"/fp-data/control/",i,"__",mission,"_solo.txt"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
      
      # log event 
      log4r::levellog(logger, 'INFO', paste("created : ", paste0(mission,"-",i,".csv")))
      
      # counter handling for the last file
      if (maxPoints + addmax > nrow(df@data) & fin == FALSE) {
        oldmax    <- maxPoints - 2
        maxPoints <- nrow(df@data)
        minPoints <- oldmax
        addmax    <- maxPoints - minPoints
        fin <- TRUE
      } else {
        minPoints <- maxPoints - 2
        maxPoints <- maxPoints + addmax
        
      }
    }
  }
}

# export data to DJI exchange format 
# (1) controls with respect to waypoint number and/or battery lifetime the splitting of the mission files to seperate task files
# (2) checking the return to home and fly to start of the misson tracks with respect to the obstacles to generate a save start and end of a task
calcDjiTask <- function(df, mission, nofiles, maxPoints, p, logger, rth, trackSwitch=FALSE, dem, maxAlt, projectDir, workingDir,locationName,runDir) {
  minPoints <- 1
  addmax    <- maxPoints
  if (maxPoints > nrow(df@data)) {maxPoints <- nrow(df@data)}
  # store launch position and coordinates we need them for the rth calculations
  row1      <- df@data[1,1:(ncol(df@data))]
  launchLat <- df@data[1,1]
  launchLon <- df@data[1,2]
  dem       <- raster(dem)
  
  # due to reprojection recalculate teh launch position and altitude
  launch_pos <- as.data.frame(cbind(launchLat,launchLon))
  sp::coordinates(launch_pos) <- ~launchLon+launchLat
  sp::proj4string(launch_pos) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  launchAlt <- raster::extract(dem,launch_pos,layer = 1, nl = 1)  
  
  # for each of the splitted task files
  for (i in 1:nofiles) {
    cat(paste0("create ",i, " of ",nofiles, " control files...\n"))  
    
    # for safety issues we need to generate synthetic climb and sink waypoints 
    # take current start position of the partial task
    startLat <- df@data[minPoints + 1,1] # minPoints+1 because of adding the endpoint of the task
    startLon <- df@data[minPoints + 1,2]
    
    # take current end position of split task
    endLat <- df@data[maxPoints,1]
    endLon <- df@data[maxPoints,2]
    
    # generate flight lines from launch to start and launch to end point of splitted task
    home  <- sp_line(c(launchLon,endLon),c(launchLat,endLat),"home",runDir=runDir)
    start <- sp_line(c(launchLon,startLon),c(launchLat,startLat),"start",runDir=runDir)
    
    # calculate minimum rth altitude for each line by identifying max altitude
    #homeRth<-max(unlist(raster::extract(dem,home)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    #startRth<-max(unlist(raster::extract(dem,start)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    maxAltHomeFlight  <- raster::extract(dem,home, fun = max, na.rm = TRUE,layer = 1, nl = 1) - launchAlt + as.numeric(p$flightAltitude)
    maxAltStartFlight <- raster::extract(dem,start,fun = max, na.rm = TRUE,layer = 1, nl = 1) - launchAlt + as.numeric(p$flightAltitude)
    
    # get the max position of the flightlines
    homemaxpos  <- maxpos_on_line(dem,home)
    startmaxpos <- maxpos_on_line(dem,start)
    
    # log the positions
    log4r::levellog(logger, 'INFO', paste("maxaltPos    rth : ", paste0("mission file: ",i," ",homemaxpos[2]," ",homemaxpos[1])))
    log4r::levellog(logger, 'INFO', paste("maxaltPos 2start : ", paste0("mission file: ",i," ",startmaxpos[2]," ",startmaxpos[1])))
    
    # calculate rth and 2start headings
    homeheading  <- geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a = 6378137, f = 1/298.257223563)
    startheading <- geosphere::bearing(c(launchLon,launchLat),c(startLon,startLat), a = 6378137, f = 1/298.257223563)
    
    # generate home max alt waypoint
    heading   <- homeheading
    altitude  <- maxAltHomeFlight + 0.1*maxAltHomeFlight
    latitude  <- homemaxpos[1,2]
    longitude <- homemaxpos[1,1]
    
    # generate ascent waypoint to realize save fly home altitude
    homemaxrow <- cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # generate maximum altitude wp on the way to the mission start
    heading     <- startheading
    altitude    <- maxAltStartFlight + 0.1*maxAltStartFlight
    latitude    <- startmaxpos[1,2]
    longitude   <- startmaxpos[1,1]
    startmaxrow <- cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # calculate rth ascent from last task position
    pos <- calcNextPos(endLon,endLat,homeheading,10)
    
    # generate rth waypoints
    heading   <- homeheading
    altitude  <- maxAltHomeFlight
    latitude  <- pos[2]
    longitude <- pos[1]
    
    # generate ascent waypoint to realize save fly home altitude
    ascentrow <- cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # generate home position with heading and altitude
    homerow <- cbind(row1[1:2],altitude,heading,row1[5:length(row1)])
    
    # generate launch to start waypoint to realize save fly home altitude
    pos       <- calcNextPos(launchLon,launchLat,startheading,10)
    heading   <- startheading
    altitude  <- as.numeric(p$flightAltitude)
    latitude  <- pos[2]
    longitude <- pos[1]
    startrow  <- cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # calculate rth ascent from last task position
    pos            <- calcNextPos(longitude,latitude,startheading,10)
    heading        <- startheading
    altitude       <- maxAltStartFlight
    latitude       <- pos[2]
    longitude      <- pos[1]
    startascentrow <- cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # extract the dataframe from the sp point object
    DF <- df@data[(as.numeric(minPoints) + 1):maxPoints,]
    
    # add the 6 safety points to each dataframe (i.e. task)
    DF = rbind(startmaxrow,DF)
    DF = rbind(startascentrow,DF)
    DF = rbind(startrow,DF)
    DF = rbind(DF,ascentrow)
    DF = rbind(DF,homemaxrow)
    DF = rbind(DF,homerow)
    
    #if (maxPoints>nrow(DF)){maxPoints<-nrow(DF)}
    utils::write.csv(DF[,1:(ncol(DF) - 2)],file = paste0(projectDir,"/",locationName ,"/", workingDir,"/fp-data/control/",mission,i,"_dji.csv"),quote = FALSE,row.names = FALSE)
    
    log4r::levellog(logger, 'INFO', paste("created : ", paste0(strsplit(projectDir,"/tmp")[[1]][1],"/log/",mission,"-",i,".csv")))
    
    minPoints <- maxPoints - 2 # -2 for overlapping end of task WPs
    maxPoints <- maxPoints + addmax
    
    if (maxPoints > nrow(df@data)) {
      maxPoints <- nrow(df@data)
      addmax <- maxPoints - minPoints
    }
  }
}



# (DJI only) create the full argument list for one waypoint
makeUavPoint <- function(pos, uavViewDir, group, p, header = FALSE, sep = ",") {
  # create the value lines
  if (!header) {
    # create camera action arguments
    action <- ""
    for (i in seq(1:length(p$task[,1]))) { 
      action <- paste0(action,p$task[i,]$x[1],sep)
    }
    # create waypoint plus camera options
    tmp <-    paste0(pos[1],sep,pos[2],sep,pos[2],sep,pos[1],
                     sep,as.character(p$flightAltitude),
                     sep,as.character(uavViewDir),
                     sep,as.character(p$curvesize),
                     sep,as.character(p$rotationdir),
                     sep,as.character(p$gimbalmode),
                     sep,as.character(p$gimbalpitchangle),
                     sep,action,
                     group)
  }
  # create the header
  else {
    action <- ""
    for (i in seq(1:length(p$task[,1]))) { 
      action <- paste0(action,p$task[i,]$actionNames[1],sep)
    }
    tmp <-    paste0("lon",sep,"lat",sep,"latitude",sep,"longitude",sep,
                     "altitude",sep,
                     "heading",sep,
                     "curvesize",sep,
                     "rotationdir",sep,
                     "gimbalmode",sep,
                     "gimbalpitchangle",sep,
                     action,"id")    
  }
}



calcSurveyArea <- function(surveyArea,projectDir,logger) {
  
  # check and read mission area coordinates
  if (is.null(surveyArea)) {
    log4r::levellog(logger, 'FATAL', '### external flight area file or coordinates missing - dont know what to to')
    stop("### external flight area file or coordinates missing - don't know what to to")
  }
  else {
    # import flight area if provided by an external vector file
    if (class(surveyArea) == "numeric" & length(surveyArea) >= 8) {
      surveyArea <- surveyArea
    }
    else if (class(surveyArea) == "numeric" & length(surveyArea) < 8) {
      log4r::levellog(logger, 'FATAL', "### you did not provide a launching coordinate")
      stop("### you did not provide a launching coordinate")
    }
    else {
      #file.copy( from = surveyArea, to = file.path(projectDir,"data"))
      test <- try(flightBound <- readExternalFlightBoundary(surveyArea))
      if (class(test) != "try-error") {
        surveyArea <- flightBound 
      } else {
        log4r::levellog(logger, 'FATAL', "### can not find/read input file")        
        stop("### could not read surveyArea file")
      }
    }
  }
  return(surveyArea)
}

# imports the survey area from a json or kml file
importSurveyArea <- function(fN) {
  # read shapefile
  #if (path.expand(extension(fN)) == ".json") 
  #  flightBound <- rgdal::readOGR(dsn = path.expand(fN), layer = "OGRGeoJSON",verbose = FALSE)
  # else if (path.expand(extension(fN)) != ".kml" ) 
  #    flightBound <- rgdal::readOGR(dsn = path.expand(dirname(fN)), layer = tools::file_path_sans_ext(basename(fN)), pointDropZ = TRUE, verbose = FALSE)
  #else if (path.expand(extension(fN)) == ".kml" ) {
  tmp <- sf::st_read(path.expand(fN))
  flightBound = as(tmp, "Spatial")
  #flightBound <- rgdal::readOGR(dsn = path.expand(fN), layer = tools::file_path_sans_ext(basename(fN)), pointDropZ = TRUE, verbose = FALSE)    
  #  }
  flightBound@data <- as.data.frame(cbind(1,1,1,1,1,-1,0,-1,1,1,1))
  names(flightBound@data) <- c("Name", "description", "timestamp", "begin", "end", "altitudeMode", "tessellate", "extrude", "visibility", "drawOrder", "icon")
  return(flightBound)
}

# imports the survey area from a list of for coordinates
readExternalFlightBoundary <- function(fN, extend = FALSE) {
  flightBound <- importSurveyArea(fN)
  sp::spTransform(flightBound, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  if (extend) {
    x <- raster::extent(flightBound)
    
    # first flightline used for length and angle of the parallels
    lon1 <- x@xmin # startpoint
    lat1 <- x@ymin # startpoint
    lon2 <- x@xmin # endpoint
    lat2 <- x@ymax # endpoint
    lon3 <- x@xmax # crosswaypoint
    lat3 <- x@ymax # crosswaypoint
    if (class(flightBound) == "SpatialPolygonesDataFrame") {
      launchLon  <- flightBound@polygons[[1]]@Polygons[[1]]@coords[4,1] 
      launchLat <- flightBound@polygons[[1]]@Polygons[[1]]@coords[4,2]  
    } else if (class(flightBound) == "SpatialLinesDataFrame") {
      launchLon <- flightBound@lines[[1]]@Lines[[1]]@coords[7,1] 
      launchLat <- flightBound@lines[[1]]@Lines[[1]]@coords[7,2]
    }
  } else {
    if (class(flightBound) == "SpatialPolygonesDataFrame") {
      lon1 <- flightBound@polygons[[1]]@Polygons[[1]]@coords[1,1] 
      lat1 <- flightBound@polygons[[1]]@Polygons[[1]]@coords[1,2] 
      
      lon2 <- flightBound@polygons[[1]]@Polygons[[1]]@coords[2,1] 
      lat2 <- flightBound@polygons[[1]]@Polygons[[1]]@coords[2,2] 
      
      lon3 <- flightBound@polygons[[1]]@Polygons[[1]]@coords[3,1] 
      lat3 <- flightBound@polygons[[1]]@Polygons[[1]]@coords[3,2] 
      
      launchLon  <- flightBound@polygons[[1]]@Polygons[[1]]@coords[4,1] 
      launchLat <- flightBound@polygons[[1]]@Polygons[[1]]@coords[4,2]       
    }
    if (class(flightBound) == "SpatialLinesDataFrame") {
      
      tr<-try(lon3 <- flightBound@lines[[1]]@Lines[[1]]@coords[5,1],silent = TRUE)
      if (class(tr)== "try-error"){
        lon1 <- flightBound@lines[[1]]@Lines[[1]]@coords[1,1] 
        lat1 <- flightBound@lines[[1]]@Lines[[1]]@coords[1,2] 
        
        lon2 <- flightBound@lines[[1]]@Lines[[1]]@coords[2,1] 
        lat2 <- flightBound@lines[[1]]@Lines[[1]]@coords[2,2] 
        
        lon3 <- flightBound@lines[[1]]@Lines[[1]]@coords[3,1] 
        lat3 <- flightBound@lines[[1]]@Lines[[1]]@coords[3,2]
        
        launchLon <- flightBound@lines[[1]]@Lines[[1]]@coords[4,1] 
        launchLat <- flightBound@lines[[1]]@Lines[[1]]@coords[4,2]  
      } else {
      lon1 <- flightBound@lines[[1]]@Lines[[1]]@coords[1,1] 
      lat1 <- flightBound@lines[[1]]@Lines[[1]]@coords[1,2] 
      
      lon2 <- flightBound@lines[[1]]@Lines[[1]]@coords[3,1] 
      lat2 <- flightBound@lines[[1]]@Lines[[1]]@coords[3,2] 
      
      lon3 <- flightBound@lines[[1]]@Lines[[1]]@coords[5,1] 
      lat3 <- flightBound@lines[[1]]@Lines[[1]]@coords[5,2]
      
      launchLon <- flightBound@lines[[1]]@Lines[[1]]@coords[7,1] 
      launchLat <- flightBound@lines[[1]]@Lines[[1]]@coords[7,2]}
    }
  }
  return(c(lat1,lon1,lat2,lon2,lat3,lon3,launchLat,launchLon))
}


#  function to start litchi as a local instance TO BE DONE
# openLitchi<- function(){
#   tempDir <- tempfile()
#   dir.create(tempDir)
#   currentfiles<-list.files(paste0(.libPaths()[1],"/uavRmp/htmlwidgets/lib/litchi"))
#   dir.create(file.path(tempDir, currentfiles[1]))
#   currentfiles<-list.files(paste0(.libPaths()[1],"/uavRmp/htmlwidgets/lib/litchi/"))
#   
#   file.copy(from=paste0(.libPaths()[1],"/uavRmp/htmlwidgets/lib/litchi"), to=file.path(tempDir), 
#             overwrite = TRUE, recursive = TRUE, 
#             copy.mode = TRUE)
#   
#   htmlFile <- file.path(tempDir, "litchi","index.html")
#   # (code to write some content to the file)
#   utils::browseURL(htmlFile)
#   
# }


# calculate the overlap factor of the camera footprints returning an heatmap
calcFovHeatmap <- function(footprint,dem) {
  p        <- split(footprint, footprint@plotOrder)
  t        <- raster::raster(nrow = nrow(dem)*2,ncol = ncol(dem)*2)
  t@crs    <- dem@crs
  t@extent <- dem@extent
  t        <- resample(dem,t)
  t[]      <- 0
  s        <- t
  
  for (i in seq(1:length(footprint))) {
    tmp <- raster::rasterize(p[[i]],t)
    s <- raster::stack(tmp, s)
  }
  fovhm <- raster::stackApply(s, indices = nlayers(s), fun = sum)
  fovhm[fovhm < 1] = NaN
  return(fovhm)
}

# create a sp polygon to estimate the pictures footprint 
taskarea <- function(p, csvFn) {
  # construct the 4th corner
  crossdir <- geosphere::bearing(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a = 6378137, f = 1/298.257223563)
  crosslen <- geosphere::distGeo(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a = 6378137, f = 1/298.257223563)
  p4       <- geosphere::destPoint(c(p$lon1,p$lat1), crossdir,crosslen)
  # create SPDF
  ID = paste0("FlightTask_",basename(csvFn[[1]]))
  rawPolygon <- sp::Polygon(cbind(c(p$lon1,p$lon2,p$lon3,p4[[1]],p$lon1),c(p$lat1,p$lat2,p$lat3,p4[[2]],p$lat1)))
  areaExtent <- sp::Polygons(list(rawPolygon), ID = ID)
  areaExtent <- sp::SpatialPolygons(list(areaExtent))
  df <- data.frame( ID = 1:length(rawPolygon), row.names = ID)
  area <- sp::SpatialPolygonsDataFrame(areaExtent, df)
  sp::proj4string(area) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  return(area)
} 

# calculates the camera footprint 
calcCamFoot <- function(lon, lat, heading, distance, flightaltitude, i, j) {
  
  t1 <- calcNextPos(lon,lat,abs(heading),1.71*flightaltitude/2)
  t2 <- calcNextPos(lon,lat,abs(heading),-1*(1.71*flightaltitude/2))
  
  yllc <- calcNextPos(t1[1],t1[2],-90 + abs(heading),1.71*flightaltitude*0.75/2)[2]
  xllc <- calcNextPos(t1[1],t1[2],-90 + abs(heading),1.71*flightaltitude*0.75/2)[1]
  ylrc <- calcNextPos(t1[1],t1[2],90  + abs(heading),1.71*flightaltitude*0.75/2)[2]
  xlrc <- calcNextPos(t1[1],t1[2],90  + abs(heading),1.71*flightaltitude*0.75/2)[1]
  
  yulc <- calcNextPos(t2[1],t2[2],-90 + abs(heading),1.71*flightaltitude*0.75/2)[2]
  xulc <- calcNextPos(t2[1],t2[2],-90 + abs(heading),1.71*flightaltitude*0.75/2)[1]
  yurc <- calcNextPos(t2[1],t2[2],90  + abs(heading),1.71*flightaltitude*0.75/2)[2]
  xurc <- calcNextPos(t2[1],t2[2],90  + abs(heading),1.71*flightaltitude*0.75/2)[1]
  
  ID = paste0("CameraExtend_",flightaltitude,"_",lon,lat)
  rawPolygon <- sp::Polygon(cbind(c(xulc,xurc,xlrc,xllc,xulc),c(yulc,yurc,ylrc,yllc,yulc)))
  tileExtend <- sp::Polygons(list(rawPolygon), ID = ID)
  tileExtend <- sp::SpatialPolygons(list(tileExtend))
  df <- data.frame( ID = 1:length(rawPolygon), row.names = ID)
  frame <- sp::SpatialPolygonsDataFrame(tileExtend, df)
  sp::proj4string(frame) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  return(frame)
} 

fp_getPresetTask <- function(param="remote") {
  # shows existing camera action presets 
  # @description 
  # NOTE: only for flightPlanMode = "waypoint")
  # preset waypoints & orthophoto
  
  
  if  (param == "multi_ortho") {
    actiontype = c(1,0,4,0,5,-60,1,0,4,90,1,0,4,180,1,0,4,270,1,0)
    flightParams <- actiontype
    task <- makeTaskParamList(flightParams[1:length(flightParams)])
  }
  # preset waypoints take vertical picture at wp
  else if (param == "simple_ortho") { 
    actiontype = c(5,-90,1,0)
    flightParams <- actiontype 
    task <- makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "simple_pano") { 
    actiontype = c(4,-180,1,0,4,-128,1,0,4,-76,1,0,4,-24,1,0,4,28,1,0,4,80,1,0,4,132,1,0,-1,0) 
    flightParams <- actiontype
    task <- makeTaskParamList(flightParams[1:length(flightParams)])
  }  # preset waypoints  take vertical picture at wp
  else if (param == "remote") { 
    actiontype = c(-1,0)
    flightParams <- actiontype
    task <- makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "treetop") { 
    actiontype = c(0,1000,5,-90,1,0,1,0,5,-70,1,0,4,-90,1,0,4,90,5,-30,-1,0,-1,0,-1,0,-1,0,-1,0)
    flightParams <- actiontype
    task <- makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "nothing") { 
    actiontype = c(-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0)
    flightParams <- actiontype
    task <- makeTaskParamList(flightParams[1:length(flightParams)])
  }
  return(task)
}

# create and recalculates all arguments for a drone waypoint
makeFlightParam <- function(surveyArea,flightParams,followSurface) {
  # retrieve and recalculate the arguments to provide the flight paramaer for litchi
  validPreset     <- c("multi_ortho","simple_ortho","simple_pano","remote","treetop","nothing")
  validFlightPlan <- c("waypoints","track","manual")
  stopifnot(flightParams["presetFlightTask"] %in% validPreset)
  stopifnot(flightParams["flightPlanMode"] %in% validFlightPlan)
  
  if (followSurface == TRUE) {
    flightParams["flightPlanMode"] = "terrainTrack"
  }
  
  p <- list()
  # preset camera action at waypoints 
  task <- fp_getPresetTask(flightParams["presetFlightTask"])  
  
  # flight area coordinates either from external file or from argument list
  p$lat1 <- surveyArea[1]
  p$lon1 <- surveyArea[2]
  p$lat2 <- surveyArea[3]
  p$lon2 <- surveyArea[4]
  p$lat3 <- surveyArea[5]
  p$lon3 <- surveyArea[6]
  p$launchLat <- surveyArea[7]
  p$launchLon <- surveyArea[8]
  p$launchAltitude <- flightParams["launchAltitude"]
  # rest of the arguments  
  p$flightPlanMode <- flightParams["flightPlanMode"] # waypoints, terrainTrack track
  p$flightAltitude <- flightParams["flightAltitude"]  # planned static altitude above ground (note from starting point)
  p$curvesize <- flightParams["curvesize"]      # default may be set t0 zero
  p$rotationdir <- flightParams["rotationdir"]      # default nothing
  p$gimbalmode <- flightParams["gimbalmode"]       # default nothing 
  p$gimbalpitchangle <- flightParams["gimbalpitchangle"] # default nothing
  p$overlap <- overlap <- flightParams["overlap"]    # overlapping factor 0-1 default 0.6
  p$task <- task  # camera task
  p$followSurfaceRes <- flightParams["followSurfaceRes"]
  return(p)
}

# creates task paramter list
makeTaskParamList <- function(x) {
  actionNames <- list()
  j <- 1
  for (i in seq(1:(length(x)/2)) ) {
    actionNames[j]     <- paste0("actiontype",i)
    actionNames[j + 1] <- paste0("actionparam",i) 
    j <- j + 2
  }
  return(cbind(actionNames,x))
  
}

# calculate a new position from given lat lon
calcNextPos <- function(lon,lat,heading,distance) {
  p <- geosphere::destPoint(c(lon,lat), heading, distance)
  return(c(p[1],p[2]))
}

dumpFile = function(filepath) {
  con = file(filepath, "r")
  while (TRUE) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

# calculates the overall flight distance
calcTrackDistance <- function(fliAltRatio,flightAltitude,factor=1.71) {
  
  trackDistance <- (fliAltRatio*(factor*flightAltitude))
  
}

calculateFlightTime <- function(maxFlightTime, windCondition, maxSpeed, uavOptimumspeed, flightLength, totalTrackdistance, picRate, logger) {
  # wind speed adaption for reducing the lifetime of the battery - roughly the Beaufort scale is used
  
  if (windCondition == 0) {
    windConditionFactor <- 1.0
  } else if (windCondition == 1) {
    windConditionFactor <- 0.9
  } else if (windCondition == 2) {
    windConditionFactor <- 0.7
  } else if (windCondition == 3) {
    windConditionFactor <- 0.4
  } else if (windCondition == 4) {
    windConditionFactor <- 0.1
  } else if (windCondition > 5) {
    windConditionFactor <- 0.0
    log4r::levellog(logger, 'INFO', "come on, it is a uav not the falcon...")  
    stop("come on, it is a cheap uav not a falcon...")
  }
  
  # log preset picture rate sec/pic
  log4r::levellog(logger, 'INFO', paste("original picture rate: ", picRate,"  (sec/pic) "))    
  
  #   # calculate speed & time parameters  
  if (maxSpeed > uavOptimumspeed) {
    maxSpeed <- uavOptimumspeed
    log4r::levellog(logger, 'INFO',paste( "optimum speed forced to ", uavOptimumspeed," km/h \n"))
    cat("\n optimum speed forced to ", uavOptimumspeed," km/h \n")
  }
  # calculate time need to fly the task
  rawTime <- round(((flightLength/1000)/maxSpeed)*60,digits = 1)
  
  # calculate the corresponding (raW)  time intevall for each picture
  picIntervall <- round(rawTime*60/(flightLength/totalTrackdistance), digits = 1)
  log4r::levellog(logger, 'INFO', paste("initial speed estimation  : ", round(maxSpeed, digits = 1),   "  (km/h)      "))
  while (picIntervall < picRate) {
    maxSpeed <- maxSpeed - 1
    rawTime <- round(((flightLength/1000)/maxSpeed)*60, digits = 1)
    rawTime <- rawTime*windConditionFactor
    picIntervall <- round(rawTime*60/(flightLength/totalTrackdistance),digits = 1)
    log4r::levellog(logger, 'INFO', paste("decrease speed to  : ", round(maxSpeed,digits = 1),   "  (km/h)      "))
  }
  
  # APPLY battery lifetime loss by windspeed
  maxFlightTime <- maxFlightTime*windConditionFactor
  return(c(rawTime,maxFlightTime,maxSpeed,picIntervall))
}


# assign launching point 
launch2flightalt <- function(p, lns, uavViewDir, launch2startHeading, uavType) {
  launchPos <- c(p$launchLon,p$launchLat)
  if (uavType == "djip3") {lns[length(lns) + 1] <- makeUavPoint(launchPos, uavViewDir, group = 99, p)}
  if (uavType == "solo")  {lns[length(lns) + 1] <- makeUavPointMAV(lat = launchPos[2], lon = launchPos[1], head = uavViewDir, group = 99)}
  pOld <- launchPos
  pos <- calcNextPos(pOld[1],pOld[2],launch2startHeading,10)
  if (uavType == "djip3") {lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)}
  if (uavType == "solo")  {lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2], lon = pos[1], head = uavViewDir, group = 99)}
  return(lns)
}

# generate raw mavtree list 
MAVTreeCSV <- function(flightPlanMode, 
                       trackDistance, 
                       logger, 
                       p, 
                       dem, 
                       maxSpeed = maxSpeed/3.6, 
                       circleRadius,
                       df,
                       takeOffAlt,
                       projectDir,
                       runDir) {
  mission <- p$locationName
  
  minPoints <- 1
  #nofiles<- ceiling(rawTime/batteryTime)
  nofiles <- 1
  maxPoints <- nrow(df@data)
  mp <- maxPoints
  
  a  <- 0
  b  <- 0
  c  <- 3
  d  <- "0.000000"
  e  <- "0.000000"
  f  <- "0.000000"
  g  <- "0.000000"
  id <- 99
  j  <- 1
  
  #if (maxPoints > nrow(df@data)) {maxPoints<-nrow(df@data)}
  # store launchposition and coordinates we need them for the rth calculations
  for (i in 1:nofiles) {
    launchLon<- p$launchLon
    launchLat<- p$launchLat
    launchAlt<- p$launchAltitude
    # for safety issues we need to generate synthetic climb and sink waypoints 
    # take current start position of the split task
    startLat <- df@data[minPoints ,8]
    startLon <- df@data[minPoints ,9]
    
    # take current end position of split task
    endLat <- df@data[maxPoints - 1,8]
    endLon <- df@data[maxPoints - 1,9]
    
    # depending on DEM/DSM sometimes there are no data Values
    # generate flight lines from launch to start and launch to end point of splitted task
    home  <- sp_line(c(launchLon,endLon),c(launchLat,endLat),"Home",runDir=runDir)
    start <- sp_line(c(launchLon,startLon),c(launchLat,startLat),"Start",runDir=runDir)
    
    # calculate minimum rth altitude for each line by identifing max altitude
    homeRth  <- raster::extract(dem,home, fun = max,na.rm = TRUE,layer = 1, nl = 1) - launchAlt + as.numeric(p$flightAltitude)
    startRth <- raster::extract(dem,start,fun = max,na.rm = TRUE,layer = 1, nl = 1) - launchAlt + as.numeric(p$flightAltitude)
    
    # add 10% of flight altitude as safety buffer
    homeRth  <- homeRth  + 0.1 * homeRth
    startRth <- startRth + 0.1 * startRth
    
    if (startRth < 50) {
      takeOffAlt <- as.numeric(startRth)
    } else {
      takeOffAlt <- 50
    }
    
    # get the max position of the flightlines
    homemaxpos  <- maxpos_on_line(dem,home)
    startmaxpos <- maxpos_on_line(dem,start)
    
    # calculate heading 
    homeheading  <- geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a = 6378137, f = 1/298.257223563)
    startheading <- geosphere::bearing(c(launchLon,launchLat),c(startLon,startLat), a = 6378137, f = 1/298.257223563)
    
    names(df) <-c("CURRENT_WP","COORD_FRAME","COMMAND","PARAM1","PARAM2","PARAM3","PARAM4","latitude","longitude","altitude","id")
    # write and re-read waypoints
    sep<-"\t"
    keeps <- c("CURRENT_WP","COORD_FRAME","COMMAND","PARAM1","PARAM2","PARAM3","PARAM4","latitude","longitude","altitude","id")
    df@data<-df@data[keeps]
    utils::write.table(df@data[minPoints:maxPoints,1:(ncol(df@data))],file = file.path(runDir,"tmp.csv"),quote = FALSE,row.names = FALSE,sep = "\t")
    lns <- data.table::fread(file.path(runDir,"tmp.csv"), skip=1L, header = FALSE,sep = "\n", data.table = FALSE)
    lnsnew<-data.frame()
    
    # create default header line  
    lnsnew[1,1] <- "QGC WPL 110"
    # create homepoint 
    # lnsnew[2,1] <- mavCmd(id = 0, 
    #                       cmd = 179,
    #                       lat = p$launchLat,
    #                       lon = p$launchLon,
    #                       alt = p$launchAltitude) 
    # TAKEOFF
    lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 1, 
                                               cmd = 22,   # 22 MAV_CMD_NAV_TAKEOFF
                                               alt = round(takeOffAlt,6))
    
    # SPEED taxiway
    lnsnew[length(lnsnew[,1]) + 1,1] <-       mavCmd(id = 2, 
                                                     cmd = 178, 
                                                     p2 = round(maxSpeed,6))
    
    # ascent2start WP
    lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 3, 
                                               cmd = 16,
                                               lat = round(calcNextPos(launchLon,launchLat,startheading,15)[2],6),
                                               lon = round(calcNextPos(launchLon,launchLat,startheading,15)[1],6),
                                               alt = round(takeOffAlt + (startRth - takeOffAlt) / 2 ,6))
    # maxStartPos WP
    lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = 4, 
                                               cmd = 16,
                                               lat = round(startmaxpos[1,2],6),
                                               lon = round(startmaxpos[1,1],6),
                                               alt = round(startRth,6))
    
    lnsnew[length(lnsnew[,1]) + 1,1] <-       mavCmd(id = 5, 
                                                     cmd = 115, 
                                                     p1 = "0.000000")
    # create "normal" waypoints
    lc <- length(lnsnew[,1])
    
    # task WP & task speed
    for (j in  seq(1,(length(lns[,1]) - 1))) {
      sp<- str_split(pattern = "\t",string = lns[ceiling(j),])
      if (sp[[1]][3] == 19){
        # circle waypoint
        lnsnew[j + lc , 1] <- mavCmd(id = j  + lc - 2,
                                     cmd = 19,
                                     p1 = "12.00000",    
                                     lat = sp[[1]][8],
                                     lon = sp[[1]][9],
                                     alt = sp[[1]][10])
      }
      
      else {
        # up or down waypoint
        lnsnew[j + lc,1] <-   mavCmd(id = j + lc - 2, 
                                     cmd = 16,
                                     lat = sp[[1]][8],
                                     lon = sp[[1]][9],
                                     alt = sp[[1]][10])
      }
    }
    
    
    # MAV fly to launch sequence
    # RTH altitude TODO
    lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = as.character(length(lns[,1]) + 7), 
                                               cmd = 30,
                                               alt = round(homeRth,0))
    # SPEED max return speed
    lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = as.character(length(lns[,1]) + 8), 
                                               cmd = 178,
                                               p2 = round(maxSpeed,6))
    # trigger RTL event
    lnsnew[length(lnsnew[,1]) + 1,1] <- mavCmd(id = as.character(length(lns[,1]) + 9), 
                                               cmd = 20)
    
    # write the control file
    utils::write.table(lnsnew, 
                       paste0(strsplit(projectDir,"/fp-data/run")[[1]][1],"/fp-data/control/",i,"__",mission,"_solo.txt"), 
                       sep="\t", 
                       row.names=FALSE, 
                       col.names=FALSE, 
                       quote = FALSE,
                       na = "")
    
    # log event 
    log4r::levellog(logger, 'INFO', paste("created : ", paste0(mission,"-",i,".csv")))
    
    minPoints<-maxPoints
    maxPoints<-maxPoints+mp
    if (maxPoints>nrow(df@data)){
      maxPoints<-nrow(df@data)
    }
  }
  
}

# read text file contasining x,y coordinates
readTreeTrack<- function(treeTrack){
  tTkDF<-utils::read.csv(treeTrack,sep=",",header = TRUE)
  sp::coordinates(tTkDF) <- ~X+Y
  sp::proj4string(tTkDF) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
  #sp::proj4string(tTkDF) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84 +no_defs")
  #tTkDF<-sp::spTransform(tTkDF, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  return(tTkDF)
}

# calculate a obstacle free flight path for a given list of coordinates

makeFlightPathT3 <- function(treeList,
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
                             runDir){
  # due to RMD Check Note
  uavViewDir<-pos<-workingDir<-trackSwitch<-NULL
  if (is.null(demFn)) {
    log4r::levellog(logger, 'WARN', "CAUTION!!! no DEM file provided")
    stop("CAUTION!!! no DEM file provided")}
  cat("preprocessing DSM data...\n")
  if (class(demFn)[1] == "character") rst <- raster::raster(demFn)
  # read local dem file
  if (class(rst)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    rundem<-rst
    proj <- projection(rundem)
    demll <- gdalUtils::gdalwarp(srcfile = demFn, dstfile = file.path(runDir,"demll.tif"), overwrite=TRUE,  t_srs = "+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE ) 
    
    rundem <- raster::crop(demll,
                           extent(flightArea@bbox[1] - 0.00421,
                                  flightArea@bbox[3] + 0.00421,
                                  flightArea@bbox[2] - 0.00421,
                                  flightArea@bbox[4] + 0.00421))
    raster::writeRaster(rundem,file.path(runDir,"tmpdem.tif"),overwrite = TRUE)
    demll <- rundem
    dem  <- demll
  }
  
  # demll <- gdalwarp(srcfile = file.path(runDir,"tmpdem.tif"), 
  #                   dstfile = file.path(runDir,"demll.tif"), 
  #                   t_srs = "+proj=longlat +datum=WGS84 +no_defs",
  #                   overwrite=TRUE,
  #                   output_Raster = TRUE )  
  
  #demll <- setMinMax(demll)
  
  
  lns <- list()
  fileConn <- file(file.path(runDir,"treepoints.csv"))
  for (i in 1:nrow(treeList) ) {
    if (uavType == "djip3") {
      forward <- geosphere::bearing(treeList@coords[i,], treeList@coords[i + 1,], a = 6378137, f = 1/298.257223563)
      backward <- geosphere::bearing(treeList@coords[i + 1,], treeList@coords[i,], a = 6378137, f = 1/298.257223563)
      p$task <- fp_getPresetTask("treetop")
      lns[length(lns) + 1] <- makeUavPoint(treeList@coords[i,], forward, p, group = 99)
      p$task <- fp_getPresetTask("nothing")
      posUp  <- calcNextPos(treeList@coords[i,][1], treeList@coords[i,][2], heading = forward, distance = p$climbDist)
      lns[length(lns) + 1] <- makeUavPoint(posUp, forward, p, group = 1)
      posDown <- calcNextPos(treeList@coords[i + 1,][1], treeList@coords[i + 1,][2], backward, distance = p$climbDist)
      lns[length(lns) + 1] <- makeUavPoint(posDown, forward, p, group = 1)
      writeLines(unlist(lns), fileConn)
    }
    else if (uavType == "solo") {
      cat("calculating flight corridors according to position ",i," of ",nrow(treeList),"\r")
      lp <- sp_point(p$launchLon,p$launchLat,"LaunchPos")
      
      if (p$launchAltitude == -9999){
        tmpalt <- raster::extract(dem,lp,layer = 1, nl = 1)  
        p$launchAltitude <- as.numeric(tmpalt)
        # otherwise take it from the parameter set
      } else 
      {
        p$launchAltitude <- as.numeric(p$launchAltitude)
      }
      # extract all waypoint altitudes
      altitude <- as.data.frame(raster::extract(demll,treeList,layer = 1, nl = 1))
      altitude<-as.matrix(altitude)
      # get maximum altitude of the flight corridors
      maxAlt <- max(altitude,na.rm = TRUE)
      
      if (i >= nrow(treeList)){
        forward <- geosphere::bearing(treeList@coords[i,],lp, a = 6378137, f = 1/298.257223563)
        backward <- geosphere::bearing(lp,treeList@coords[i,], a = 6378137, f = 1/298.257223563)
        posUp <- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],heading = forward,distance = p$climbDist)
        posDown <- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],backward,distance = p$climbDist)
        #posNext <- lp@coords
        
      } else {
        forward <- geosphere::bearing(treeList@coords[i,],treeList@coords[i + 1,], a = 6378137, f = 1/298.257223563)
        backward <- geosphere::bearing(treeList@coords[i + 1,],treeList@coords[i,], a = 6378137, f = 1/298.257223563)
        posUp <- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],heading = forward,distance = p$climbDist)
        posDown <- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],backward,distance = p$climbDist)
        #posNext <- c(treeList@coords[i + 1,][1],treeList@coords[i + 1,][2])
      }
      
      if (i == 1 ) {
        down_smtlon <- lp@coords[1]
        down_smtlat <- lp@coords[2]
        up_smtlon <- treeList@coords[i + 1,][1]
        up_smtlat <- treeList@coords[i + 1,][2]
        
      } else if (i > 1 & i < nrow(treeList)) {
        down_smtlon <- treeList@coords[i - 1,][1]
        down_smtlat <- treeList@coords[i - 1,][2]
        up_smtlon <- treeList@coords[i + 1,][1]
        up_smtlat <- treeList@coords[i + 1,][2]
      } else if (i == nrow(treeList)){
        up_smtlon <- lp@coords[1]
        up_smtlat <- lp@coords[2]
        down_smtlon <- treeList@coords[i - 1,][1]
        down_smtlat <- treeList@coords[i - 1,][2]
      }
      
      
      seg_max_toDown <- get_seg_fparams(demll,
                                        start = c(down_smtlon,down_smtlat),
                                        target = c(treeList@coords[i,][1],treeList@coords[i,][2]),
                                        p,
                                        runDir=runDir)
      
      
      tree_Alt <- get_point_fparams(demll,
                                    point = c(treeList@coords[i,][1],treeList@coords[i,][2]),
                                    p,
                                    radius =circleRadius
      )      
      
      
      seg_max_toNext <- get_seg_fparams(demll,
                                        start = c(posDown[1],posDown[2]),
                                        target = c(up_smtlon,up_smtlat),
                                        p,
                                        runDir=runDir)      
      
      # create DOWN WP
      lns[length(lns) + 1] <- makeUavPointMAV(lat = posDown[2],
                                              lon = posDown[1],
                                              head = 0.000000,
                                              alt =  as.numeric(seg_max_toDown[1]),
                                              group = 1) 
      
      # create TREE WP
      lns[length(lns) + 1] <- makeUavPointMAV( cmd = 19,  
                                               p1 = 1.00000,    
                                               p3 = round(5,6),
                                               #p4 = round(90,6),
                                               lat = treeList@coords[i,][2],
                                               lon = treeList@coords[i,][1],
                                               head = 0.000000,
                                               alt= round(tree_Alt,6),
                                               group = 99) 
      # create UP WP
      lns[length(lns) + 1] <- makeUavPointMAV(lat = posUp[2],
                                              lon = posUp[1],
                                              head = forward,
                                              alt =  as.numeric(seg_max_toNext[1]),
                                              group = 1) 
      # write the down tree up triple to a
      writeLines(unlist(lns), fileConn)
    }
  }
  close(fileConn)
  if (uavType == "djip3") {
    cat("calculating DEM related stuff\n")
    djiDF <- utils::read.csv(file.path(runDir,"treepoints.csv"),sep = ",",header = FALSE)
    names(djiDF) <- unlist(strsplit( makeUavPoint(pos,uavViewDir,group = 99,p,header = TRUE,sep = ' '),split = " "))
    sp::coordinates(djiDF) <- ~lon+lat
    sp::proj4string(djiDF) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    result <- getAltitudes(demll ,djiDF,p,followSurfaceRes = 5,logger,projectDir,locationName,flightArea)
    writeDjiTreeCSV(result[[2]],p$locationName,1,94,p,logger,round(result[[4]],digits = 0),trackSwitch,result[[3]],result[[6]],projectDir)
    return(result)
    
  } else if (uavType == "solo") {
    cat("getting altitudes...\n")
    df <- utils::read.csv(file.path(runDir,"treepoints.csv"),sep = "\t",header = FALSE)
    names(df) <- c("a","b","c","d","e","f","g","latitude","longitude","altitude","id","autocont","lat","lon")
    sp::coordinates(df) <- ~lon+lat
    sp::proj4string(df) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # sp::spTransform(treeList,CRSobj = CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    # result <- getAltitudes(demll ,df,p,followSurfaceRes = 5,logger,projectDir,locationName,flightArea)
    
    
    MAVTreeCSV(flightPlanMode = "track",
               trackDistance = 10000,
               logger = logger,
               p = p,
               dem = demll,
               maxSpeed = p$maxSpeed,
               circleRadius,
               df = df,
               takeOffAlt,
               projectDir,
               runDir)
    names(df) <- c("CURRENT_WP","COORD_FRAME","COMMAND","PARAM1","PARAM2","PARAM3","PARAM4","latitude","longitude","altitude","id", "AUTOCONTINUE")
    keeps<- c("CURRENT_WP","COORD_FRAME","COMMAND","PARAM1","PARAM2","PARAM3","PARAM4","latitude","longitude","altitude", "AUTOCONTINUE")
    df@data<-df@data[keeps]
    return(df)
  }
}

# get altitudes for dji flightpath
getAltitudes <- function(demll ,df,p,followSurfaceRes,logger,projectDir,locationName,flightArea) {
  
  # extract all waypoint altitudes
  altitude <- as.data.frame(raster::extract(demll,df,layer = 1, nl = 1))
  names(altitude) <- "altitude"
  altitude<-as.matrix(altitude)
  # get maximum altitude of the task area
  maxAlt <- max(altitude,na.rm = TRUE)
  log4r::levellog(logger, 'INFO', paste("maximum DEM Altitude : ", maxAlt," m"))
  # if no manually provided launch altitude exist get it from DEM
  pos <- as.data.frame(cbind(p$launchLat,p$launchLon))
  sp::coordinates(pos) <- ~V2+V1
  sp::proj4string(pos) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  if (p$launchAltitude == -9999){
    tmpalt <- raster::extract(demll,pos,layer = 1, nl = 1)  
    p$launchAltitude <- as.numeric(tmpalt)
    # otherwise take it from the parameter set
  } else 
  {
    p$launchAltitude <- as.numeric(p$launchAltitude)
  }
  log4r::levellog(logger, 'INFO', paste("launching Altitude : ", p$launchAltitude," m"))
  launchAlt <- p$launchAltitude
  # calculate the flight altitude shift due to launching and max altitude
  p$flightAltitude <- as.numeric(p$flightAltitude) + (maxAlt - as.numeric(launchAlt))
  p$aboveTreeAlt <- as.numeric(p$aboveTreeAlt) + (maxAlt - as.numeric(launchAlt))
  fa<-as.numeric(p$flightAltitude) + (maxAlt - as.numeric(launchAlt))
  
  rthFlightAlt <- p$flightAltitude
  p$rthAltitude <- rthFlightAlt
  log4r::levellog(logger, 'INFO', paste("rthFlightAlt : ", rthFlightAlt," m"))
  rawAltitude <- altitude
  # altitude <- altitude + as.numeric(p$flightAltitude) - maxAlt
  # df$altitude <- altitude
  taltitude <- as.data.frame(rawAltitude + as.numeric(p$aboveTreeAlt) - maxAlt)
  taltitude$id <- df@data$id
  
  #TODO flight altitude tree altitude
  tmp <- df@data
  tmp$altitude[tmp$id == 99 ] <- taltitude$altitude[taltitude$id == 99 ]
  #tmp$altitude[tmp$id == 1 ] <- taltitude$altitude[taltitude$id == 1 ]
  df$altitude <- tmp$altitude
  return <- c(pos,df,demll,rthFlightAlt,launchAlt,maxAlt)
  names(return) <- c("lp","wp","dsm","rth","la","xa")
  return(return)
}

# get launch position coordinates
readLaunchPos <- function(fN,extend=FALSE){
  if (class(fN) != "numeric") {
    flightBound <- importSurveyArea(fN)
    launchLon <- flightBound@polygons[[1]]@Polygons[[1]]@coords[1,1] 
    launchLat <- flightBound@polygons[[1]]@Polygons[[1]]@coords[1,2] 
  }
  else{
    # create SPDF
    # points from scratch
    coords = cbind(fN[1],fN[2])
    launchPos = sp::SpatialPoints(coords)
    launchPos = SpatialPointsDataFrame(coords, as.data.frame("name"))
    # promote data frame to spatial
    sp::proj4string(launchPos) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  }
  return(launchPos)
}

# # export data to external format deals with the splitting of the mission files
# writeDjiTreeCsv <-function(df,mission){
#   # max numbers of waypoints is 99
#   nofiles<-ceiling(nrow(df@data)/96)
#   maxPoints<-96
#   minPoints<-1
#   maxFlightLength <- 15
#   
#   for (i in 1:nofiles) {
#     if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
#     utils::write.csv(df@data[minPoints:maxPoints,1:(ncol(df@data)-2)],file = paste0(strsplit(projectDir,"/fp-data/run")[[1]][1],"/fp-data/control/",i,"__",mission,"__dji.csv"),quote = FALSE,row.names = FALSE)
#     minPoints<-maxPoints
#     maxPoints<-maxPoints+96
#     
#     if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
#   }
# }

# export data to xternal format deals with the splitting of the mission files
writeDjiTreeCSV <-function(df,mission,nofiles,maxPoints,p,logger,rth,trackSwitch=FALSE,dem,maxAlt,projectDir){
  minPoints<-1
  if (maxPoints > nrow(df@data)) {maxPoints<-nrow(df@data)}
  # store launchposition and coordinates we need them for the rth calculations
  row1<-df@data[1,1:(ncol(df@data))]
  
  launchLat<-p$launchLat
  launchLon<-p$launchLon
  
  for (i in 1:nofiles) {
    # take current start position of the split task
    startLat<-df@data[minPoints,1]
    startLon<-df@data[minPoints,2]
    # take current end position of split task
    endLat<-df@data[maxPoints,1]
    endLon<-df@data[maxPoints,2]
    # generate flight lines from launch to start and launch to end point of splitted task
    yhome <- c(launchLat,endLat)
    xhome <- c(launchLon,endLon)
    ystart <- c(launchLat,startLat)
    xstart <- c(launchLon,startLon)
    start<-SpatialLines(list(Lines(Line(cbind(xstart,ystart)), ID="start")))
    home<-SpatialLines(list(Lines(Line(cbind(xhome,yhome)), ID="home")))
    sp::proj4string(home) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    sp::proj4string(start) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # calculate minimum rth altitude for each line by identifying max altitude
    homeRth<-max(unlist(raster::extract(dem,home,layer = 1, nl = 1)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    startRth<-max(unlist(raster::extract(dem,start,layer = 1, nl = 1)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    
    # calculate rth heading 
    homeheading<-geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    startheading<-geosphere::bearing(c(startLon,startLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    
    altitude<-startRth
    latitude<-  launchLat<-p$launchLat
    longitude<-launchLon<-p$launchLon
    heading<-startheading
    # generate ascent waypoint to realize save fly home altitude
    rowStart<-cbind(latitude,longitude,altitude,heading,row1[5:ncol(df@data)])
    
    # calculate rth ascent from last task position
    pos<-calcNextPos(endLon,endLat,homeheading,10)
    
    # generate rth waypoints
    heading<-homeheading
    altitude<-homeRth
    latitude<-pos[2]
    longitude<-pos[1]
    # generate ascent waypoint to realize save fly home altitude
    ascentrow<-cbind(latitude,longitude,altitude,heading,rowStart[5:ncol(df@data)])
    # generate home position with heading and altitude
    homerow<-cbind(rowStart[1:2],altitude,heading,rowStart[5:ncol(df@data)])
    # genrate launch to start waypoint to realize save fly home altitude
    heading<-homeheading
    altitude<-startRth
    startrow<-cbind(rowStart[1:2],altitude,heading,rowStart[5:ncol(df@data)])
    
    # append this three points to each part of the splitted task
    DF<-df@data[minPoints:maxPoints,]
    DF = rbind(startrow,DF)
    DF = rbind(DF,ascentrow)
    DF = rbind(DF,homerow)
    
    #if (maxPoints>nrow(DF)){maxPoints<-nrow(DF)}
    utils::write.csv(DF[,1:(ncol(DF)-2)],file = paste0(strsplit(projectDir,"/tmp")[[1]][1],"/fp-data/control/",i,"__",mission,"__dji.csv"),quote = FALSE,row.names = FALSE)
    log4r::levellog(logger, 'INFO', paste("created : ", paste0(strsplit(projectDir,"/tmp")[[1]][1],"/fp-data/control/",i,"__",mission,"__dji.csv")))
    minPoints<-maxPoints
    maxPoints<-maxPoints+94
    
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
}



# get UTM zone of given longitude
long2UTMzone <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}
rad2deg <- function(rad) {(rad * 180) / (pi)}

deg2rad <- function(deg) {(deg * pi) / (180)}

# inserts a row in a dataframe
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# create the full argument list for one waypoint in MAV format
makeUavPointMAV<- function(lat=0.000000,lon=0.000000,alt=100.0,head=0,wp=0,cf=3,cmd=16,p1=0.000000,p2=0.000000,p3=0.000000,p4=0.000000,autocont=1,dif=22,header=FALSE,sep="\t",speed="11.8",group,lf=FALSE,raw=TRUE){
  sep <- "\t"
  #<CURRENT WP> <COORD FRAME> <COMMAND> <PARAM1> <PARAM2> <PARAM3> <PARAM4> <PARAM5/X/LONGITUDE> <PARAM6/Y/LATITUDE> <PARAM7/Z/ALTITUDE> <AUTOCONTINUE>
  p4<-head
  id<-group
  dif<-dif
  heading<-head
  altitude<-alt
  latitude<-lat
  longitude<-lon
  if(raw){
    wpLine<-paste0(wp,sep,cf,sep,cmd,sep,
                   sprintf("%.6f", round(p1,6)),sep,
                   sprintf("%.6f", round(p2,6)),sep,
                   sprintf("%.6f", round(p3,6)),sep,
                   sprintf("%.6f", round(p4,6)),sep,
                   round(latitude,6),sep,
                   round(longitude,6),
                   sep,round(altitude,1),sep,
                   id,sep,autocont,sep,round(latitude,6),sep,round(longitude,6))
  } else {
    wpLine<-paste0(wp,sep,cf,sep,cmd,sep,p1,sep,p2,sep,p3,sep,p4,sep,round(latitude,digits=6),sep,round(longitude,6),sep,round(altitude,1),sep,id,sep,autocont) 
  }
  if (lf) {
    LF<-"\n"
  } else {
    LF<-NULL}
  
  # CREATE NORMAL WAYPOINT
  
  return(wpLine)    
}

# create the full argument list for one waypoint in MAV format
#makeUavPoint_MAV<- function(coord=NULL,heading=NULL,group=NULL,p=NULL,header=FALSE,sep="\t",speed="11.8"){
#  # create the value lines
#  if (!header){
#    # CREATE NORMAL WAYPOINT
#    tmp <-    paste0("0",sep,"3",sep,"16",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,pos[2],sep,pos[1],sep,pos[2],sep,pos[1],sep,as.character(p$flightAltitude),sep,group,sep,"1\n")
#    
#  }
#  # create the header
#}


mavCmd <- function(id,wp=0,cf="3",cmd="82",p1="0.000000",p2="0.000000",p3="0.000000",p4="0.000000",lon="0.000000",lat="0.000000",alt="0.000000",autocont="1"){
  sep <- "\t"
  #<INDEX> <CURRENT WP> <COORD FRAME> <COMMAND> <PARAM1> <PARAM2> <PARAM3> <PARAM4> <PARAM5/X/LONGITUDE> <PARAM6/Y/LATITUDE> <PARAM7/Z/ALTITUDE> <AUTOCONTINUE>
  return(paste0(id,sep,wp,sep,cf,sep,cmd,sep,p1,sep,p2,sep,p3,sep,p4,sep,lat,sep,lon,sep,alt,sep,autocont))
}

# getting true for odd numbers
is.odd <- function(x) x %% 2 != 0

# extract highest altitude position and agl of a single track
get_seg_fparams <- function(dem,
                              start,
                              target,
                              p,
                            runDir){
  # depending on DEM/DSM sometimes there are no data values
  startAlt<-p$launchAltitude
  seg  <- sp_line(c(start[1],target[1]),c(start[2],target[2]),"seg",runDir=runDir)
  seg_utm <- sp::spTransform(seg,CRSobj =  paste0("+proj=utm +zone=",long2UTMzone(seg@bbox[1])," +datum=WGS84"))
  seg_buf<-rgeos::gBuffer(spgeom = seg_utm,width = 5.0)
  seg_buf <- sp::spTransform(seg_buf,CRSobj = "+proj=longlat +datum=WGS84 +no_defs" )
  # calculate minimum rth altitude for each line by identifing max altitude
  seg_flight_altitude  <- raster::extract(dem,seg_buf, fun = max,na.rm = TRUE,layer = 1, nl = 1) - startAlt + as.numeric(p$flightAltitude)
  
  # add 10% of flight altitude as safety buffer
  #seg_flight_altitude  <- seg_flight_altitude  + 0.1 * seg_flight_altitude
  
  
  # get the max position of the flightlines
  seg_max_pos  <- maxpos_on_line(dem,seg)
  
  
  # calculate heading 
  #                                  
  seg_heading  <- geosphere::bearing(c(target[1],target[2]),c(start[1],start[2]), a = 6378137, f = 1/298.257223563)
  
  return (c(seg_flight_altitude,seg_max_pos,seg_heading,seg))
}

# extract highest altitude position and agl of a position with a defined radius
get_point_fparams <- function(dem,

                                point,
                                p, 
                                radius= 5.0){
  # depending on DEM/DSM sometimes there are no data values
  startAlt<-p$launchAltitude
  seg  <- sp_point(point[1],point[2],"point")
  seg_utm <- sp::spTransform(seg,CRSobj =  paste0("+proj=utm +zone=",long2UTMzone(seg@bbox[1])," +datum=WGS84"))
  seg_buf<-rgeos::gBuffer(spgeom = seg_utm,width = radius)
  seg_buf <- sp::spTransform(seg_buf,CRSobj = "+proj=longlat +datum=WGS84 +no_defs" )
  # calculate minimum rth altitude for each line by identifing max altitude
  seg_flight_altitude  <- raster::extract(dem,seg_buf, fun = max,na.rm = TRUE,layer = 1, nl = 1) - startAlt + as.numeric(p$aboveTreeAlt)
  
  # calculate heading 
  return (c(seg_flight_altitude))
}


setProjStructure <- function(projectDir,
                             locationName, 
                             
                             flightAltitude,
                             uavType,
                             cameraType,
                             surveyArea,
                             demFn,
                             copy,
                             ft="A",
                             runDir){
  workingDir <- tools::file_path_sans_ext(basename(as.character(surveyArea)))
  
  projRootDir <- file.path(projectDir, locationName, workingDir)
  
  if (ft=="A") ftype <- "_AREA"
  if (ft=="P") ftype <- "_POSITION"
  flight_date <- format(Sys.time(), "%Y_%m_%d")
  taskName <-paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                          "__",
                          cameraType,"__", 
                          tools::file_path_sans_ext(basename(as.character(surveyArea))),"_", 
                          ftype,
                   "__",
                    flightAltitude,"m")
  
  initProj(projRootDir= projRootDir, projFolders=c("fp-data/log/",
                                                   "fp-data/control/",
                                                   "fp-data/run/",
                                                   "fp-data/data/",
                                                   "img-data/FLIGHT1/log",
                                                   "img-data/FLIGHT1/level0",
                                                   "img-data/FLIGHT1/level1",
                                                   "img-data/FLIGHT1/level2"))
  
  
  # copy planning data: DSM and flightplanning area to projcet folder
  if (!is.numeric(surveyArea)) {
    file.copy(surveyArea, paste0(file.path(projRootDir, "fp-data/data")),overwrite = TRUE)
    surveyArea <- paste0(file.path(projRootDir, "fp-data/data"), "/", basename(surveyArea))
    
  }
  # copy DSM to datafolder
  if (!is.null(demFn) & copy ) {
    file.copy(demFn, paste0(file.path(projRootDir, "fp-data/data"), "/", basename(demFn)))
    demFn <- paste0(file.path(projRootDir, "fp-data/data"), "/", basename(demFn))
    
  }
  # setting R environ temp folder to the current working directory
  Sys.setenv(TMPDIR = file.path(projRootDir, "fp-data/run"))
  
  # set R working directory
  #setwd(file.path(projRootDir, "fp-data/run"))
  
  # set common read write permissions
  #Sys.chmod(list.dirs("../.."), "777")
  
  
  # generate misson control filename
  csvFn <-paste(file.path(projRootDir, "fp-data/control/"),paste0(taskName, ".csv"),sep = .Platform$file.sep)
  
  makeGlobalVar(name = "runDir",value = file.path(projRootDir,"fp-data/run/"))
  return(c(csvFn, taskName, workingDir,projRootDir))
}