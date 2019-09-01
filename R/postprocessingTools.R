
#' extract exif information
#' @description  extract exif
#'
#' @param position default is TRUE the files are selected by their relative position
#' @param distance default is 30 m distance of the image centres 
#' @param path path to the images files
#'
#' @examples
#' wp <- system.file("extdata", "MAVLINK_waypoints.txt", package = "uavRmp")
#' \dontrun{
#' solo_upload( missionFile = wp)
#' }
#' @export drop_images
#'               

drop_images <- function(path = NULL ,
                        position = TRUE, 
                        distance = 30,
                        image_format = "jpg"){
  
  if (image_format != "jpg" & image_format !="raw") return()
  
  if (Sys.info()["sysname"] == "Linux") {
    perlPath  <- try(system2("find", paste("/usr/bin"," ! -readable -prune -o -type f -executable -iname 'exiftool' -print"), stdout = TRUE))
    perlPath<- file.path(R.utils::getAbsolutePath(perlPath),"exiftool -n")
    
  }
  if (Sys.info()["sysname"] == "Windows") {
    print(":: searching for the exiftool... this may take a while")
    perlPath <- try(system(paste0("cmd.exe /c dir /B /S C:\\","exiftool.exe"),intern = TRUE))
    perlPath<- shortPathName(file.path(R.utils::getAbsolutePath(perlPath)))
    perlPath<-paste(perlPath,"-n")
    
  }
  
  if (length(perlPath) > 0) exiftool<-perlPath[2]
  else {
    print("No exiftools installed... pls install and retry")
    return()
  }
  
  
  
  flist<-list()
  exifInfos<-list()
  imageLat<-list()
  imageLon<-list()
  flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path),"*.JPG")))
  print(paste("retrieving exif information for ",length(flist), "files this may take a while "))
  for (j in seq(1:length(flist))) {
    cat(paste(j,"\b"))
    
    #svMisc::progress(j,progress.bar = TRUE)
    exifInfos[[j]]<-system(paste(exiftool,flist[j]),intern = TRUE)  
    gpsposRow<-grep("GPS Position",exifInfos[[j]],useBytes = TRUE)
    imageLat[[j]] <- strsplit(strsplit(exifInfos[[j]][gpsposRow],":")[[1]][2]," ")[[1]][2]
    imageLon[[j]] <- strsplit(strsplit(exifInfos[[j]][gpsposRow],":")[[1]][2]," ")[[1]][3]
    if (j==length(flist)) message(done)
  }
  
  cat("done")
  
  
  library(geosphere)
  distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  write.csv(dat2, 'Exifdata.csv',
            row.names = F)
  
  
  return()
}
