# karim is the blue djinn
if (!isGeneric('read_gpx ')) {
  setGeneric('read_gpx ', function(x, ...)
    standardGeneric('read_gpx '))
}

#' Read GPX file
#' 
#' Read a GPX file. By default, it reads all possible GPX layers, and only returns shapes for layers that have any features.
#' 
#' @param file a GPX filename (including directory)
#' @param layers vector of GPX layers. Possible options are \code{"waypoints"}, \code{"tracks"}, \code{"routes"}, \code{"track_points"}, \code{"route_points"}. By dedault, all those layers are read.
#' @return  if the layer has any features a sp object is returned.
#' @note cloned from tmap
#' @examples 
#' ## for visualisation we are using mapview
#' require(mapview)
#' ## assign  GPX file
#' gpxFN <- system.file("extdata", "flighttrack.gpx", package = "uavRmp")
#' 
#' ## read it
#' gpx <- read_gpx(gpxFN, layers=c("tracks"))
#' 
#' ## plot it
#' mapview::mapview(gpx)
#' 
#' @export read_gpx

read_gpx <- function(file, layers=c("waypoints", "tracks", "routes", "track_points", "route_points")) {
  if (!all(layers %in% c("waypoints", "tracks", "routes", "track_points", "route_points"))) stop("Incorrect layer(s)", call. = FALSE)
  
  # check if features exist per layer
  suppressWarnings(hasF <- sapply(layers, function(l) {
    ogrInfo(dsn = file, layer=l)$have_features
  }))
  
  if (!any(hasF)) stop("None of the layer(s) has any features.", call. = FALSE)
  
  res <- lapply(layers[hasF], function(l) {
    readOGR(dsn = file, layer=l, verbose=FALSE)
  })
  names(res) <- layers[hasF]
  
  if (sum(hasF)==1) {
    res[[1]]
  } else {
    res
  }
}


# Check projection of objects according to their keywords -------

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


# 
# 
#' create an spatiallineobject from 2 points
#' @description
#' create an spatiallineobject from 2 points, optional export as shapefile
#' @param Y_coords Y/lat coordinates 
#' @param X_coords X/lon coordinates
#' @param ID id of line
#' @param proj4 projection
#' @param export write shafefile default = F 
#' @export
#' 
#' @examples 
#' ## creating sp spatial point object
#' line <- sp_line(c(8.770367,8.771161,8.771536),c(50.815172,50.814743,50.814875),ID="go for it")
#' 
#' ## plot it
#' raster::plot(line)
#' 
sp_line <- function(Y_coords,
                    X_coords,
                    ID,
                    proj4="+proj=longlat +datum=WGS84 +no_defs",
                    export=FALSE) {   
  line <- SpatialLines(list(Lines(Line(cbind(Y_coords,X_coords)), ID = ID)))
  sp::proj4string(line) <- CRS(proj4)
  if (export) {
    writeLinesShape(line,paste0(ID,"home.shp"))
  }
  return(line)
}
#' create an spatialpointobject from 1 points
#' @description
#' create an spatial point object from 1 point and optionally export it as a shapefile
#' @param lon lon
#' @param lat lat
#' @param proj4 projection
#' @param ID name of point
#' @param export write shafefile default = F 
#' @export
#' @examples 
#' ## creating sp spatial point object
#' point <- sp_point(8.770362,50.815240,ID="Faculty of Geographie Marburg")
#' 
#' ## plot it
#' raster::plot(point)
#' 
sp_point <- function(lon,
                     lat,
                     ID="point",
                     proj4="+proj=longlat +datum=WGS84 +no_defs",
                     export=FALSE) {
  point = cbind(lon,lat)
  point = sp::SpatialPoints(point)
  point = SpatialPointsDataFrame(point, as.data.frame(ID))
  sp::proj4string(point) <- CRS(proj4)
  if (export) {
    writeLinesShape(ID,paste0(ID,".shp"))
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
#' ## load DEM/DSM 
#' dem <- raster::raster(system.file("extdata", "mrbiko.tif", package = "uavRmp"))
#' 
#' ## generate extraction line object
#' line <- sp_line(c(8.66821,8.68212),c(50.83939,50.83267),ID="Highest Position")
#' 
#' ## extract highest position
#' maxpos_on_line(dem,line)  
#' 
 
maxpos_on_line <- function(dem,line){
  mask <- dem
  raster::values(mask) <- NA
  #...update it with the altitude information of the flightline
  mask  <- raster::rasterize(line,mask)
  mask2 <- mask*dem
  # and find the position of the max altitude
  idx = raster::which.max(mask2)
  maxPos = raster::xyFromCell(mask2,idx)
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
fun_whichmax <- function(mask,value) { 
  raster::xyFromCell(value,which.max(mask * value))
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
