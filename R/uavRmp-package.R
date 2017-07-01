#' Unmanned Aerial Vehicle R based Mission Planner
#'
#' The package provides some mission planning functionality for dealing with Unmanned Aerial Vehicles
#'
#' @name uavRmp
#' @docType package
#' @title Unmanned Aerial Vehicle R based Mission Planner  - awesome Mission Planner for complex monitoring flights
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#'
#' @import stringr zoo foreach sp raster htmlwidgets htmltools rgeos rgdal gdalUtils tools maptools mapview parallel velox log4r rgrass7 devtools roxygen2 sf methods
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
#'  
#'
#' @useDynLib uavRmp
#' @keywords package
#' 
NULL
#'
#' @docType data
#' @name mrbiko
#' @title DEM data set of Marburg-Biedenkopf
#' @description DEM data set resampled to 20 m resolution
#' @format \code{"raster::raster"}
#'
NULL

