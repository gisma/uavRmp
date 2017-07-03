#' The package provides some mission planning functionality for dealing with Unmanned Aerial Vehicles
#'
#' @description The package provides some mission planning functionality for dealing #' with Unmanned Aerial Vehicles
#' @note It is important to keep in mind that all binaries need to be installed 
#' correctly on your system. 
#' @name uavRmp
#' @docType package
#' @title UAV R Based Mission Planner
#' @author Chris Reudenbach Lars Opgenoorth Sebastian Richter
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#' 
#' 
#'
#' @import stringr 
#' @import zoo
#' @import sp
#' @import raster
#' @import htmlwidgets
#' @import htmltools
#' @import rgeos
#' @import rgdal
#' @import gdalUtils
#' @import tools
#' @import maptools
#' @import mapview
#' @import log4r
#' @import devtools
#' @import roxygen2
#' @import sf 
#' @import methods
#' @import devtools
#' @importFrom geosphere bearing
#' @importFrom geosphere distGeo
#' @importFrom geosphere destPoint
#' @importFrom igraph clusters
#' @importFrom igraph graph
#' @importFrom igraph V
#' @importFrom data.table fread
#' @importFrom spatial.tools create_blank_raster 
#' @import log4r
#' @import caret
#' @useDynLib uavRmp
#' @keywords package
#' 
NULL
#' DEM data set of Marburg-Biedenkopf
#' @docType data
#' @name mrbiko
#' @title DEM data set of Marburg-Biedenkopf
#' @description DEM data set resampled to 20 m resolution
#' @format \code{"raster::raster"}
#' @keywords datasets
#' @source \code{uav flight marburg universitsy forest}
NULL
#'

