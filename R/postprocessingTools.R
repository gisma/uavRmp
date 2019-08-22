#' systematically delete files from a given directory
#' @description  #' systematically delete files from a given directory
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
                        image_format = "jpg",
                        perlcmd= "C:\\Strawberry\\perl\\bin\\perl"){

if (image_format != "jpg" & image_format !="raw") return()
  
flist<-list()
flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path),"*.JPG")))
dat <- exifr::read_exif(path,recursive = TRUE,args = "-n")
ret2 <- dplyr::select(dat,
               SourceFile, DateTimeOriginal,
               GPSLongitude, GPSLatitude,
               GPSTimeStamp)
library(geosphere)
distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
write.csv(dat2, 'Exifdata.csv',
          row.names = F)
exifinfo <- read_exif(path = path,recursive = TRUE,tags = c("gpslatitude", "gpslongitude","gpstimestamp"), args = "-n")
ret<-system2("C:\\Strawberry\\perl\\bin\\perl","C:\\Users\\solo\\Documents\\R\\win-library\\3.4\\exifr\\exiftool\\exiftool.pl -filename -gpslatitude -gpslongitude -T -n X:\\nature40\\2019_05_16_core14\\images > X:/nature40/2019_05_16_core14/images/out.txt",stdout = TRUE)

return(flist)
}