#' The package provides some mission planning functionality for dealing with Unmanned Aerial Vehicles
#'
#' @description The package provides some mission planning functionality for dealing with Unmanned Aerial Vehicles. The focus is set on an easy to use workflow for planning autonomous obstacle avoiding surveys of rtf-UAVs to retrieve aerial or spot related data. It provides either intermediate flight control files for the DJI Litchi compatible UAVs or ready to upload control files for the pixhawk based flightcontroller as used in the Yuneec H520 or the outdated 3DR Solo. Additionally it contains some useful tools for digitizing and data manipulation.

#' @note It is important to keep in mind that all auxilliary external binaries  like GDAL or SAGA need to be installed properly.
#' correctly on your system. 
#' @name uavRmp
#' @docType package
#' @title UAV Mission Planner
#' @author Chris Reudenbach Lars Opgenoorth Sebastian Richter Florian Detsch Hanna Meyer Marvin Ludwig
#' \cr
#' *Maintainer:* Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#' 
#' 
#'

#' @importFrom rlist list.cbind
#' @importFrom link2GI linkGDAL
#' @importFrom geosphere bearing
#' @importFrom geosphere distGeo
#' @importFrom geosphere destPoint
#' @importFrom methods as
#' @import log4r
#' @import dplyr
#' @import sf
#' @keywords internal
#' @aliases uavRmp-package
"_PACKAGE"
NULL
#' DJI image of a survey flight
#' @docType data
#' @name tutdata_dji
#' @title DJI image of a survey flight
#' @description DJI image of a survey flight
#' @format `"terra::rast"`
#' @keywords datasets
#' @source `Faculty of Geography UAV derived data from Marburg University Forest first campaign`
NULL
#' DEM data set of Marburg-Biedenkopf
#' @docType data
#' @name tutdata_dem
#' @title DEM data set of Marburg-Biedenkopf
#' @description DEM data set resampled to 20 m resolution
#' @format `"terra::rast"`
#' @keywords datasets
#' @source `Faculty of Geography UAV derived data from Marburg University Forest first campaign`
NULL
#'
#' MAVLINK waypoint example data 
#' @docType data
#' @name tutdata_waypoints
#' @title MAVLINK waypoint example data
#' @description Waypoint file
#' @keywords datasets
#' @source `Faculty of Geography UAV derived data from Marburg University Forest first campaign`
NULL
#'
#' Virtual object position  coordinates example data 
#' @docType data
#' @name tutdata_position
#' @title position example data
#' @description position data for planning a single flight task with focus on known objects
#' @keywords datasets
#' @source `Faculty of Geography UAV derived data from Marburg University Forest first campaign`
NULL
#'
#' GPX example data 
#' @docType data
#' @name tutdata_flighttrack
#' @title GPX example data 
#' @description GPX example data as derived by a 3DR Solo flight
#' @keywords datasets
#' @source `Faculty of Geography UAV derived data from Marburg University Forest first campaign`
NULL

#' Flight area planning example data 
#' @docType data
#' @name tutdata_flightarea
#' @title Flight area planning example data 
#' @description Flight area planning example data as typically needed for planning an autonomous survey flight task
#' @keywords datasets
#' @source `Faculty of Geography Marburg`
NULL

#' Flight area planning Qgroundcontrol survey data 100 m AGL
#' @docType data
#' @name tutdata_qgc_survey
#' @title Flight area planning Qgroundcontrol planning file for a 100m relative to launch survey flight using a GoPro Hero4
#' @description Flight area planning example data as typically needed for planning an autonomous survey flight task. The task is planned with the QGroundcontrol survey tool.
#' @keywords datasets
#' @source `Faculty of Geography Marburg`
NULL

#' Flight area planning Qgroundcontrol survey data 30 m AGL
#' @docType data
#' @name tutdata_qgc_survey30m
#' @title Flight area planning Qgroundcontrol planning file for a 30m follow terrain survey flight with the DJI Air 2S
#' @description Flight area planning example data as typically needed for planning an autonomous survey flight task. The task is planned with the QGroundcontrol survey tool.
#' @keywords datasets
#' @source `Faculty of Geography Marburg`
NULL