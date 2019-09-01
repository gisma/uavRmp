
#' extract exif information
#' @description  extract exif
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
    print(":: No exiftools installed... pls install and retry")
    return()
  }
  
  
  
  flist<-list()
  exifInfos<-list()
  imageLat<-list()
  imageLon<-list()
  flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path),"*.JPG")))
  print(paste(":: retrieving exif information from ",length(flist), "files - this may take a while..."))
  fn<-file.path(R.utils::getAbsolutePath(path),paste0(strsplit(path,.Platform$file.sep)[[1]][length(strsplit(path,.Platform$file.sep)[[1]])-1],".csv"))
  for (j in seq(1:length(flist))) {
    cat(paste("reading exif from image No.: ",j,"\r"))
    #svMisc::progress(j,progress.bar = TRUE)
    exifInfos[[j]]<-system(paste(exiftool,flist[j]),intern = TRUE)  
    gpsposRow<-grep("GPS Position",exifInfos[[j]],useBytes = TRUE)
    imageLat[j] <- strsplit(strsplit(exifInfos[[j]][gpsposRow],":")[[1]][2]," ")[[1]][2]
    imageLon[j] <- strsplit(strsplit(exifInfos[[j]][gpsposRow],":")[[1]][2]," ")[[1]][3]
   # write.csv2(x = exifInfos[[j]],file=fn,append = TRUE)
  }
  
  message("done")
  write.csv2(x = exifInfos,file=fn)
  
  # library(geosphere)
  # distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  # write.csv(dat2, 'Exifdata.csv',
  #           row.names = F)
  # 
  
  return(exifInfos)
}
