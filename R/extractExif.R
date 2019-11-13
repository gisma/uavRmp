#' Select UAV Images Information
#' @description extract all and returns specific exif information from a list of images
#' 
#' @param path path to the images files
#'
#' @return data.frame of image positions and travel distance
#' 

#' 
#'
#' @export


extractExifr <- function(path){
  
  
  exifInfo <- exifr::read_exif(path, recursive = TRUE, tags = c("SourceFile", "Directory", "FileName", "DateTimeOriginal",
                                                         "GPSLongitude", "GPSLatitude", "GPSAltitude"))
  
  
  # timestamp as POSIXct, order images by date
  exifInfo$DateTime <- as.POSIXct(exifInfo$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S")
  exifInfo <- exifInfo[order(exifInfo$DateTime),]
  
  # calculate travel distance between two timestamps
  exifInfo$distdiff <- 0
  exifInfo$timediff <- 0
  for (i in seq(nrow(exifInfo)-1)){
    exifInfo$distdiff[i+1]<-geosphere::distm(c(exifInfo$GPSLongitude[i],exifInfo$GPSLatitude[i]),
                                             c(exifInfo$GPSLongitude[i+1],exifInfo$GPSLatitude[i+1]),
                                             fun = distGeo)
    exifInfo$timediff[i+1]<-exifInfo$DateTime[i+1]-exifInfo$DateTime[i]
  }
  
  return(exifInfo)
  
}