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
#' @export read_gpx
#' @note cloned from tmap
#' 

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


if (!isGeneric('xyz2tif')) {
  setGeneric('xyz2tif', function(x, ...)
    standardGeneric('xyz2tif'))
}
#' Read and Convert xyz DEM/DSM Data as typically provided by the public authorities
#' 
#' @description
#' Read xyz data and generate a raster  \code{Raster*} object.  
#' 
#' @param xyzFN ASCII tect file with xyz values
#' @param epsgCode EPSG Code default is  "25832"

#' 
#' 
#' @examples 
#'\dontrun{
#' # get some typical data as provided by the public authority of Bavaria
#' setwd(tempdir())
#' url<-"http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip("testdata.zip",junkpaths = TRUE,overwrite = TRUE)
#' # convert it 
#' xyz2tif("DGM_1.g01dgm","25832")
#' # plot it
#' r<-raster::raster("DGM_1.tif")
#' raster::plot(r)
#'} 
#' @export xyz2tif
#' 

xyz2tif <- function(xyzFN=NULL,  epsgCode ="25832"){
  # read data 
  xyz<-data.table::fread(xyzFN)
  cat("write it to",paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),"\n")
  cat("this will probably take a while...\n")
  r <- raster::rasterFromXYZ(xyz,crs=sp::CRS(paste0("+init=epsg:",epsgCode)))
  # write it to geotiff
  raster::writeRaster(r, paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),overwrite=TRUE)
  cat("...finished\n")
}




raster_adjust_projection <- function(x) {
  llcrs <- "+proj=longlat +datum=WGS84 +no_defs"
  
  is.fact <- raster::is.factor(x)[1]
  
  non_proj_waning <-
    paste("supplied", class(x)[1], "has no projection information!", "\n",
          "provide a correctly georeferenced data raster object or 'GDAL File")
  
  if (is.fact) {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(llcrs)),
      method = "ngb")
    x <- raster::as.factor(x)
  } else {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(llcrs)),
      method = "bilinear")
  }
  
  return(x)
  
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
#' @param p1 coordinate of first point
#' @param p2 coordinate of second point
#' @param ID id of line
#' @param proj4 projection
#' @param export write shafefile default = F 
#' @export
#' 
sp_line <- function(p1,
                    p2,
                    ID,
                    proj4="+proj=longlat +datum=WGS84 +no_defs",
                    export=FALSE) {   
  line <- SpatialLines(list(Lines(Line(cbind(p1,p2)), ID = ID)))
  sp::proj4string(line) <- CRS(proj4)
  if (export) {
    writeLinesShape(line,paste0(ID,"home.shp"))
  }
  return(line)
}
#' create an spatialpointobject from 1 points
#' @description
#' create an spatialpointobject from 1 points, optional export as shapefile
#' @param lon lon
#' @param lat lat
#' @param proj4 projection
#' @param ID name of point
#' @param export write shafefile default = F 
#' @export
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


#' Build package manually
#' 
#' @description 
#' This function was specifically designed to build a package from local source 
#' files manually, i.e., without using the package building functionality 
#' offered e.g. by RStudio.  NOTE the default setting are focussing HRZ environment at Marburg University
#' 
#' 
#' @param dsn 'character'. Target folder containing source files; defaults to 
#' the current working directory.
#' @param pkgDir 'character'. Target folder containing the result ing package of the invoked build process. According to Marburg University pools the default is set to pkgDir="H:/Dokumente". If you want to use it in a different setting you may set pkgDir to whatever you want.
#' @param document 'logical'. Determines whether or not to invoke 
#' \code{\link{roxygenize}} with default roclets for documentation purposes.  
#' @param ... Further arguments passed on to \code{\link[devtools]{build}}. 
#' 
#' @seealso 
#' \code{\link{roxygenize}}, \code{\link[devtools]{build}},\code{\link{install.packages}}.
#' 
#' @author 
#' Florian Detsch, Chris Reudenbach
#' 
#' 
#' @examples
#' \dontrun{
#' ## when in a package directory, e.g. '~/satellite' 
#' umr_build()
#' }
#' 
#' @export umr_build
#' @name umr_build
umr_build <- function(dsn = getwd(), pkgDir="H:/Dokumente",document = TRUE, ...) {
  
  ## reset 'dsn' to 'H:/...'  
  if (length(grep("students_smb", dsn)) > 0) {
    lst_dsn <- strsplit(dsn, "/")
    chr_dsn <- unlist(lst_dsn)[3:5]
    dsn <- paste0("H:/", paste(chr_dsn, collapse = "/"))
  }
  
  ## if 'document = TRUE', create documentation 
  if (document) {
    cat("\nCreating package documentation...\n")
    roxygen2::roxygenize(package.dir = dsn, 
                         roclets = c('rd', 'collate', 'namespace'))
  }
  
  ## build package
  cat("\nBuilding package...\n")
  
  devtools::build(pkg = dsn, path = dirname(dsn), ...)
  
  
  ## install package
  cat("Installing package...\n")
  pkg <- list.files(dirname(pkgDir), full.names = TRUE,
                    pattern = paste0(basename(dsn), ".*.tar.gz$"))
  pkg <- pkg[length(pkg)]
  
  utils::install.packages(pkg, repos = NULL)
  
  return(invisible(NULL))
}

#' converts SAGA raster to R raster object
#' @description converts SAGA raster to R raster object
#' @param fn filname without extension
#' @param ext extent of the raster in R notation
#' @export
saga2r<- function(fn,ext) {
  path_run<-NULL
  gdalUtils::gdalwarp(paste0(path_run,fn,".sdat"), 
                      paste0(path_run,fn,".tif"), 
                      overwrite = TRUE,  
                      verbose = FALSE)
  x<-raster::raster(paste0(path_run,fn,".tif"))
  x@extent <- ext
  # convert to SAGA
  return(x)
}

#' converts SAGA raster to R raster object
#' @description converts SAGA raster to R raster object
#' @param x raster object
#' @param fn filname without extension
#' @export
r2saga <- function(x,fn) {
  # due to RMD Check Note
  path_run<-NULL
  raster::writeRaster(x,paste0(path_run,fn,".tif"),overwrite = TRUE)
  # convert to SAGA
  gdalUtils::gdalwarp(paste0(path_run,fn,".tif"), 
                      paste0(path_run,fn,".sdat"), 
                      overwrite = TRUE,  
                      of = 'SAGA',
                      verbose = FALSE)
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
