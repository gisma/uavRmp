#' Select UAV Images Information
#' @description extract all and returns specific exif information from a list of images
#' 
#' @param path path to the images files
#'
#' @return data.frame of image positions and travel distance
#' 

#' 
#'
#' @keywords internal


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

# getGPSAltDiff <- function(picPath,demPath){
# 
#   #system2("exiftool", paste0("-n   -tagsFromFile @ -AbsoluteAltitude+=", diff, " ",image))
#   exifInfo <- exifr::read_exif(picPath, recursive = FALSE, tags = c("SourceFile", "GPSLatitude", "GPSLongitude","GPSAltitude"))
#   pos <- as.data.frame(cbind(exifInfo$GPSLatitude,exifInfo$GPSLongitude))
#   sp::coordinates(pos) <- ~V2+V1
#   sp::proj4string(pos) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
#   demAlt = raster::extract(raster::raster(dem),pos)
#   diff= demAlt- exifInfo$GPSAltitude
#   return(diff)
# }
# 
# fixGPSAlt <- function(path,diff){
# 
# 
#   system2("exiftool", paste0("-n   -tagsFromFile @ -AbsoluteAltitude+=", diff, " ",path, "*.JPG"))
#   exifInfo <- exifr::read_exif(path, recursive = TRUE, tags = c("SourceFile", "GPSAltitude", "AbsoluteAltitude"))
#   
#   return(exifInfo)
# }
# 
# # workflow
# gpic="/media/creu/mob/franzosenwiese/images/DJI_101_0177.JPG"
# dem="~/Desktop/uav/jk/burgwald_DEM.tif"
# path = "/media/creu/mob/franzosenwiese/images/"
# getGPSAltDiff(gpic,dem)
# fixGPSAlt(path,diff)

# franzosenwiese 305.683-223.4

# exiftool_cmd <- paste("exiftool -GPSLongitudeRef=E -GPSLongitude=",latlon_exif[i,11]," -GPSLatitudeRef=N -GPSLatitude=",latlon_exif[i,10]," ","./nodejpg/",latlon_exif[i,4],".jpg",sep='')
# system(exiftool_cmd)
# 
 # exiftool -r -n -all -csv path="/media/creu/mob/christen/DCIM/105MEDIA/*.JPG > /media/creu/mob/christen/DCIM/102MEDIA/metadata.csv
 # metadata <- read.csv("/media/creu/mob/christen/DCIM/102MEDIA/metadata.csv")
 # diff = 94.5/2
# metadata$GPSAltitude = metadata$GPSAltitude + diff
# write.csv(metadata,"/media/creu/mob/christen/DCIM/102MEDIA/metadata2.csv")
# 
# exiftool -csv=metadata2.csv -gpslatituderef=N -gpslongituderef=W -gpsaltituderef=above -gpstrackref=T .
# exiftool -csv=metadata2.csv  .

# system("exiftools -n -GPSAltitudeRef=0 -tagsFromFile @ -RelativeAltitude>GPSAltitude *.JPG")
# 
# system("exiftool  -n -tagsFromFile @ -GPSAltitude+=47.25 *.JPG")
