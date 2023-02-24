# karim is the blue djinn

# 
#' create an spatiallineobject from 2 points
#' @description
#' create an spatiallineobject from 2 points, optional export as shapefile
#' @param Y_coords Y/lat coordinates 
#' @param X_coords X/lon coordinates
#' @param ID id of line
#' @param proj4 projection
#' @param export write shafefile default = F 
#' @param runDir `character` runtime folder 
#' @export
#' 
#' @examples 
#' \dontrun{
#' ## creating sp spatial point object
#'line <- sp_line(c(8.770367,8.771161,8.771536),
#'                 c(50.815172,50.814743,50.814875),
#'                 runDir=tempdir())
#' 
#' ## plot it
#' plot(line)
#' }
sp_line <- function(Y_coords,
                    X_coords,
                    ID = "ID",
                    proj4="+proj=longlat +datum=WGS84 +no_defs",
                    export=FALSE,
                    runDir) {   
 ## x = st_linestring(matrix(cbind(Y_coords,X_coords),ncol=2,byrow=TRUE))
##  line<-st_as_sfc(line)
##  line <- sf::st_set_crs(line, CRS(proj4))
  line <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(Y_coords,X_coords)), ID = ID)))
  sp::proj4string(line) <- sp::CRS(proj4)
  if (export) {
    sf::st_write(sf::st_as_sf(line),file.path(runDir,paste0(ID,"home.shp")))
   #maptools::writeLinesShape(line,file.path(runDir,paste0(ID,"home.shp")))
  }
  return(line)
}
#' create an spatialpointobject from 1 point
#' @description
#' create an spatial point object from 1 point and optionally export it as a shapefile
#' @param lon lon
#' @param lat lat
#' @param proj4 projection
#' @param ID name of point
#' @param export write shafefile default = F 
#' @param runDir `character` runtime folder 
#' @export
#' @examples 
#' ## creating sp spatial point object
#' point <- sp_point(8.770362,50.815240,ID="Faculty of Geographie Marburg")
#' 
#' 
sp_point <- function(lon,
                     lat,
                     ID="point",
                     proj4="+proj=longlat +datum=WGS84 +no_defs",
                     export=FALSE,
                     runDir=runDir) {
  point = cbind(lon,lat)
  point = sp::SpatialPoints(point)
  point = sp::SpatialPointsDataFrame(point, as.data.frame(ID))
  sp::proj4string(point) <- sp::CRS(proj4)
  if (export) {
    sf::st_write(sf::st_as_sf(point),file.path(runDir,paste0(ID,"home.shp")))
    #maptools::writeLinesShape(ID,file.path(runDir,paste0(ID,".shp")))
  }
  return(point)
}

#' applies a line to a raster and returns the position of the maximum value
#' @description
#'  applies a line to a raster and returns the position of the maximum value
#' @param dem raster object
#' @param line  sp object
#' @export
#' @examples 
#' \dontrun{
#' ## load DEM/DSM 
#' dem <- terra::rast(system.file("extdata", "mrbiko.tif", package = "uavRmp"))
#' 
#' ## generate extraction line object
#' line <- sp_line(c(8.66821,8.68212),c(50.83939,50.83267),ID="Highest Position",runDir=runDir)

#' ## extract highest position
#' maxpos_on_line(dem,line)  
#' }
 
maxpos_on_line <- function(dem,line){
  mask <- dem
  terra::values(mask) <- NA
  #...update it with the altitude information of the flightline
  mask  <- terra::rasterize(terra::vect(line),mask)
  mask2 <- mask*dem
  # and find the position of the max altitude
  idx = terra::where.max(mask2)
  maxPos = terra::xyFromCell(mask2,idx[2])
  return(maxPos)
}


fun_multiply <- function(x)
{
  # Note that x is received by the function as a 3-d array:
  band1 <- x[,,1]
  band2 <- x[,,2]
  result <- band1*band2
  # The output of the function should also be a 3-d array,
  # even if it is a single band:
  result <- array(result,dim=c(dim(x)[1],dim(x)[2],1))
  
  return(result)
}


### getPopupStyle creates popup style =================================================
getPopupStyle <- function() {
  # htmlTemplate <- paste(
  #   "<html>",
  #   "<head>",
  #   "<style>",
  #   "#popup",
  #   "{font-family: Arial, Helvetica, sans-serif;width: 20%;border-collapse: collapse;}",
  #   "#popup td {font-size: 1em;border: 0px solid #85ADFF;padding: 3px 20px 3px 3px;}",
  #   "#popup tr.alt td {color: #000000;background-color: #F0F5FF;}",
  #   "#popup tr.coord td {color: #000000;background-color: #A8E6A8;}",
  #   "div.scrollableContainer {max-height: 200px;max-width: 100%;overflow-y: auto;overflow-x: auto;margin: 0px;background: #D1E0FF;}",
  #   "</style>",
  #   "</head>",
  #   "<body>",
  #   "<div class='scrollableContainer'>",
  #   "<table class='popup scrollable'>",
  #   "<table id='popup'>")
  # return(htmlTemplate)
  fl <- system.file("templates/popup.brew", package = "mapview")
  pop <- readLines(fl)
  end <- grep("<%=pop%>", pop)
  return(paste(pop[1:(end-2)], collapse = ""))
}

if ( !isGeneric("initProj") ) {
  setGeneric("initProj", function(x, ...)
    standardGeneric("initProj"))
}

#'@title Defines and creates folders and variables
#'@name initProj
#'@description Defines and creates (if necessary) all folders variables
#' set the SAGA path variables and other system variables
#' exports all variables to the global environment
#'
#'@param projRootDir  project github root directory (your github name)
#'@param projFolders list of subfolders in project
#'
#' @keywords internal
#'   


initProj <- function(projRootDir=getwd(), projFolders=c("log/","control/","run/","data/")) {
  
  # switch backslash to slash and expand path to full path
  projRootDir <- gsub("\\\\", "/", path.expand(projRootDir))  
  
  # check  tailing / and if not existing append
  if (substr(projRootDir,nchar(projRootDir) - 1,nchar(projRootDir)) != "/") {
    projRootDir <- paste0(projRootDir,"/")
  }

  # create directories if needed
  if (file.exists(paste0(projRootDir,"fp-data/log/pathes.R"))) {file.remove(paste0(projRootDir,"fp-data/log/pathes.R"))}
  for (folder in projFolders) {
    if (!file.exists(file.path(projRootDir,folder))) {
      dir.create(file.path(projRootDir,folder), recursive = TRUE)}
    value <- paste0(projRootDir,folder)
    name <- substr(folder,1,nchar(folder) )
    S<-strsplit(x =name ,split = "/")
    varName<-paste0("pto_",S[[1]][lengths(S)])
    
    writePathes(varName, value,paste0(projRootDir,"fp-data/log/pathes.R"))
    

  }
  writePSCmd(projRootDir = projRootDir)
    
}

#'@title Generates a variable with a certain value in the R environment
#'@name makeGlobalVar
#' @description  Generates a variable with a certain value in the R environment
#' @param name character string name of the variable
#' @param value character string value of the variable
#' @keywords internal
#'@examples
#' \dontrun{
#'
#' # creates the global var `pathToData` with the value `~/home/data`
#' makeGlobalVar("pathToData","~/home/data") 
#' 
#' }
#' 
makeGlobalVar <- function(name,value) {
  
    newname <- gsub("/", "_", name) 
    assign(newname, value, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global GiEnv\n")
   
}

writePathes <- function(name,value,fn) {
  
utils::write.table(paste0(name,' <- "', value,'"'),fn,quote = FALSE,row.names = FALSE, col.names = FALSE ,append = TRUE)
}

writePSCmd <- function(goal = "ortho",
                       projRootDir,
                       imgPath = "img-data/FLIGHT1/level1/rgb",
                       projName = "tmp.psx",
                       alignQuality = "2",
                       orthoRes = "0.02",
                       refPre= "PhotoScan.GenericPreselection",
                       EPSG = "4326",
                       preset_RU = "50",
                       preset_RE = "1",
                       preset_PA = "10",
                       loop_RU = "5",
                       loop_RE = "10",
                       loop_PA = "2",
                       dc_quality ="MediumQuality",
                       filter_mode = "AggressiveFiltering",
                       passes = 15) {
  
  fn<-paste0(projRootDir,"fp-data/log/basicPSWorkflow.py")
  if (goal == "ortho") goal <- "singleOrtho"
  if (goal == "dense") goal <- "singleDense"
  
  flightname<-strsplit(projRootDir,split = "/")[[1]][lengths(strsplit(projRootDir,split = "/"))]
  
  script <- paste(system.file(package="uavRmp"), "python/basicPSWorkflow.py", sep = "/")
  goal <- paste0('goal = ','"',goal,'"')
  imgPath <- paste0('imgPath = ','"',projRootDir,imgPath,'"')
  projName = paste0('projName = ','"',flightname,'.psx"')
  alignQuality = paste0("alignQuality = ",alignQuality)
  orthoRes = paste0("orthoRes = ",orthoRes)
  refPre = paste0("refPre = ",refPre)
  crs = paste0('crs = ',' PhotoScan.CoordinateSystem(','"EPSG::', EPSG,'")')
  preset_RU = paste0("preset_RU = ",preset_RU)
  preset_RE = paste0("preset_RE = ",preset_RE)
  preset_PA = paste0("preset_PA = ",preset_PA)
  loop_RU = paste0("loop_RU = ",loop_RU)
  loop_RE = paste0("loop_RE = ",loop_RE)
  loop_PA = paste0("loop_PA = ",loop_PA)
  filter_mode = paste0("filter_mode = ",paste0("PhotoScan.",filter_mode))
  dc_quality = paste0("dc_quality = ",paste0("PhotoScan.",dc_quality))
  passes = paste0("passes = ",passes)

  # now brew it
  brew::brew(script,fn)
  
  }

file_move <- function(from, to,pattern="*") {
  todir <- gsub("\\\\", "/", path.expand(to)) 
  todir <- path.expand(to)
  fromdir <- path.expand(from)
  
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  list<-list.files(path.expand(fromdir),pattern = pattern)
  result<-file.rename(from = paste0(from,"/",list),  to = todir)
  
}
#' copyDir
#' @description  copyDir copy all image data to the corresponding folder
#'
#' @param fromDir `character` a path to the image data
#' @param toProjDir `character` a path to the projRootDir
#' @param pattern  `character` a string pattern for filtering  file list 
#' @keywords internal
#' 
copyDir <- function(fromDir, toProjDir, pattern="*") {
  toDir <- gsub("\\\\", "/", path.expand(toProjDir)) 
  toDir <- path.expand(toDir)
  fromDir <- path.expand(fromDir)
  
  if (!isTRUE(file.info(toDir)$isdir)) dir.create(toDir, recursive=TRUE)
  list<-list.files(path.expand(fromDir),pattern = pattern)
  result<-file.copy(from = paste0(fromDir,"/",list),  to = toDir, overwrite = TRUE,recursive = TRUE,copy.date =TRUE)
}
createTempDataTransfer <- function (){
  tmpPath <- tempfile(pattern="007")
  dir.create(tmpPath)
  return(tmpPath)
}


vecDrawInternal <- function(tmpPath, x = NULL) {
  deps<-digiDependencies(tmpPath) 
  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'vecDraw',
    x,
    dependencies = deps,
    sizingPolicy = sizing,
    package = 'uavRmp'
  )
}

### Widget output function for use in Shiny =================================================
#
vecDrawOutput <- function(outputId, width = '100%', height = '800px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'vecDraw', width, height, package = 'uavRmp')
}

### Widget render function for use in Shiny =================================================
#   
rendervecDraw<- function(expr, env = parent.frame(), quoted = FALSE) {
  projViewOutput<-NULL
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, projViewOutput, env, quoted = TRUE)
}
# create dependencies
digiDependencies <- function(tmpPath) {
  
  data_dir <- paste0(tmpPath,sep=.Platform$file.sep)
  
  
  list(
    htmltools::htmlDependency(name = "crs",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("crs.js")),
    
    htmltools::htmlDependency(name = "jsondata",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("jsondata")),
    
    htmltools::htmlDependency(
      name = "leaflet-draw",
      version= "0.7.3",
      src = c(file = tmpPath),
      script = list("leaflet.draw.js"),
      stylesheet=list("leaflet.draw.css")
    )
    
  )
}

# create dependencies
digiDependencies <- function(tmpPath) {
  
  data_dir <- paste0(tmpPath,sep=.Platform$file.sep)
  
  
  list(
    htmltools::htmlDependency(name = "crs",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("crs.js")),
    
    htmltools::htmlDependency(name = "jsondata",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("jsondata")),
    
    htmltools::htmlDependency(
      name = "leaflet-draw",
      version= "0.7.3",
      src = c(file = tmpPath),
      script = list("leaflet.draw.js"),
      stylesheet=list("leaflet.draw.css")
    )
    
  )
}

###  creates temporary file structure for data transfer =================================================

createTempDataTransfer <- function (){
  tmpPath <- tempfile(pattern="007")
  dir.create(tmpPath)
  return(tmpPath)
}

vecDrawInternal <- function(tmpPath, x = NULL) {
  deps<-digiDependencies(tmpPath) 
  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'vecDraw',
    x,
    dependencies = deps,
    sizingPolicy = sizing,
    package = 'uavRmp'
  )
}



# workflow
# gpic="/media/creu/mob/franzosenwiese/images/DJI_101_0177.JPG"
# dem="~/Desktop/uav/jk/burgwald_DEM.tif"
# path = "/media/creu/mob/franzosenwiese/images/"
# getGPSAltDiff(gpic,dem)
# fixGPSAlt(path,diff)

#' getGPSAltDiff
#' @description  extract the difference of the exif GPS aLtitude tag and a given DEM altitude at this point 
#'
#' @param picPath `character` a path to the image 
#' @param demPath `character` a path to the DEM


#' @examples 
#' \dontrun{
#' demFN <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#' picFN <- system.file("extdata", "dji.jpg", package = "uavRmp")
#  getGPSAltDiff(picFN,demFN)
#' 
#' }
#' @keywords internal
getGPSAltDiff <- function(picPath,demPath){
  
  #system2("exiftool", paste0("-n   -tagsFromFile @ -AbsoluteAltitude+=", diff, " ",image))
  exifInfo <- exifr::read_exif(picPath, recursive = FALSE, tags = c("SourceFile", "GPSLatitude", "GPSLongitude","GPSAltitude"))
  pos <- as.data.frame(cbind(exifInfo$GPSLatitude,exifInfo$GPSLongitude))
  sp::coordinates(pos) <- ~V2+V1
  sp::proj4string(pos) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
  demAlt = as.numeric(terra::extract(terra::rast(demPath),terra::vect(pos),ID=FALSE))
  diff= demAlt- exifInfo$GPSAltitude
  return(diff)
}


#' fixGPSAlt
#' @description  fixes broken GPS altitudes in the exif data of DJI images  with the given differen value
#'
#' @param path `character`  path to the images 
#' @param diff `number` fixing value in meter
#' @param camType `character` default is "YUN" for Yuneec H520RTK alternative is "DJI"
#' 
#' @examples 
#' \dontrun{
#' 
#' picFN <- system.file("extdata", "dji.jpg", package = "uavRmp")
#  fixGPSAlt(picFN,100.0)
#' 
#' }
#' @keywords internal
#' 
#' 
fixGPSAlt <- function(path,diff,camType="YUN"){
  
  if (camType == "DJI")
  system2("exiftool", paste0("-n   -tagsFromFile @ -AbsoluteAltitude+=", diff, " ",path, "*.JPG"))
  if (camType == "YUN")
  system2("exiftool", paste0("-n   -tagsFromFile @ -GPSAltitude+=", diff, " ",path, "*.JPG"))
  exifInfo <- exifr::read_exif(path, recursive = TRUE, tags = c("SourceFile", "GPSAltitude", "AbsoluteAltitude"))
  
  return(exifInfo)
}


comp_ll_proj4 <- function(x) {
  proj <- datum <- nodefs <- "FALSE"
  allWGS84 <- as.vector(c("+init=epsg:4326", "+proj=longlat", "+datum=WGS84", "+no_defs", "+ellps=WGS84", "+towgs84=0,0,0"))
  s <- as.vector(strsplit(x," "))
  for (i in seq(1:length(s[[1]]))) {
    
    if (s[[1]][i] == "+init=epsg:4326") {
      proj <- datum <- nodefs <- "TRUE"
    }
    if (s[[1]][i] == "+proj=longlat") {
      proj <- "TRUE"
    }
    if (s[[1]][i] == "+no_defs") {
      nodefs <- "TRUE"
    }
    if (s[[1]][i] == "+datum=WGS84") {
      datum <- "TRUE"
    }
  }
  if (proj == "TRUE" & nodefs == "TRUE" &  datum == "TRUE") {
    ret <- TRUE
  } else {
    ret = FALSE
  }
  return(ret)
}
