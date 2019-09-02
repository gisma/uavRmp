
#' extract all and returns specific exif information from a list of images
#' @description  extract all and returns specific exif information from a list of images
#'
#' @param position default is TRUE the files are selected by their relative position
#' @param distance default is 30 m distance of the image centres 
#' @param path path to the images files
#' @param image_format jpg or raw
#'
#' @examples
#' wp <- system.file("extdata", "MAVLINK_waypoints.txt", package = "uavRmp")
#' \dontrun{
#' solo_upload( missionFile = wp)
#' }
#' @export 
#'               

selectImages <- function(path = NULL ,
                         position = TRUE, 
                         distance = 30,
                         image_format = "jpg"){
  
  if (image_format != "jpg" & image_format !="raw") return()
  
  if (Sys.info()["sysname"] == "Linux") {
    perlPath  <- try(system2("find", paste("/usr/bin"," ! -readable -prune -o -type f -executable -iname 'exiftool' -print"), stdout = TRUE))
    perlPath<- paste0(perlPath," -n")
    
  }
  if (Sys.info()["sysname"] == "Windows") {
    print(":: searching for the exiftool... this may take a while")
    perlPath <- try(system(paste0("cmd.exe /c dir /B /S C:\\","exiftool.exe"),intern = TRUE))
    perlPath<- shortPathName(file.path(R.utils::getAbsolutePath(perlPath)))
    perlPath<-paste(perlPath,"-n")
    
  }
  
  if (length(perlPath) > 0) exiftool<-perlPath[1]
  else {
    print(":: No exiftools installed... pls install and retry")
    return()
  }
  
  
  
  flist<-list()
  exifInfos<-list()
  imageLat<-list()
  imageLon<-list()
  imageTime<-list()
  imageDate<-list()
  flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path),"*.JPG")))
  print(paste(":: retrieving exif information from ",length(flist), "files - this may take a while..."))
  fn<-file.path(R.utils::getAbsolutePath(path),paste0(strsplit(path,.Platform$file.sep)[[1]][length(strsplit(path,.Platform$file.sep)[[1]])-1],".csv"))
  if (file.exists(fn)) {
    cat(":: file ",fn, "already exists - moved to",paste0(fn,"~\n"))
    file.rename(from = fn,to = paste0(fn,"~"))
  }
  
  
  exifInfo <- lapply(seq(length(flist)), function(j){
    cat(paste("::: reading exif from image No.: ",j,"\r"))
    #svMisc::progress(j,progress.bar = TRUE)
    img<-unlist(flist[j])
    exifInfos<-system(paste(exiftool,flist[j]),intern = TRUE)  
    gpsposRow<-grep("GPS Position",exifInfos,useBytes = TRUE)
    timeRow<-grep("Modify Date",exifInfos,useBytes = TRUE)
    warningRow<- grep("Warning",exifInfos,useBytes = TRUE)
    if (length(warningRow)==0){
      imageDate <- strsplit(strsplit(exifInfos[timeRow]," :")[[1]][2]," ")[[1]][2]
      imageTime <- strsplit(strsplit(exifInfos[timeRow]," :")[[1]][2]," ")[[1]][3]
      imageLat <- strsplit(strsplit(exifInfos[gpsposRow],":")[[1]][2]," ")[[1]][2]
      imageLon <- strsplit(strsplit(exifInfos[gpsposRow],":")[[1]][2]," ")[[1]][3]
      write.table(x = as.data.frame(exifInfos),file=fn,append = TRUE,sep = ",",row.names=F, col.names=F )
      data.frame(img=img,date =  imageDate, time=imageTime,lat=imageLat,lon=imageLon,stringsAsFactors = FALSE)
    } 
    else {
      message(":: image ",flist[j], " has invalid exif data")
    }
  })
  exifInfo <- as.data.frame(do.call("rbind", exifInfo))
  exifInfo$da<- paste0(exifInfo$date," ",exifInfo$time)
  exifInfo$time <- strptime(exifInfo$da,format = "%Y:%m:%d %H:%M:%S")
  exifInfo$lat<- as.numeric(exifInfo$lat)
  exifInfo$lon<- as.numeric(exifInfo$lon)
  
  exifInfo$dist[1]<-0
  exifInfo$timediff[1]<-0
  for (i in seq(1:(nrow(exifInfo)-1)))   {
    exifInfo$dist[i+1]<-geosphere::distm(c(exifInfo$lon[i],exifInfo$lat[i]), c(exifInfo$lon[i+1],exifInfo$lat[i+1]), fun = distGeo)
    exifInfo$timediff[i+1]<-exifInfo$time[i+1]-exifInfo$time[i]
  }
  return(exifInfo)
}
