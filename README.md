## Unmanned Aerial Vehicle R based Mission Planning - uavRmp 

<!-- badges: start -->
[![cran
checks](https://badges.cranchecks.info/worst/uavRmp.svg)](https://cran.r-project.org/web/checks/check_results_uavRmp.html)
![monthly](https://cranlogs.r-pkg.org/badges/uavRmp)
![total](https://cranlogs.r-pkg.org/badges/grand-total/uavRmp)
[![CRAN](https://www.r-pkg.org/badges/version/uavRmp?color=009999)](https://cran.r-project.org/package=uavRmp)
[![](https://img.shields.io/github/stars/gisma/uavRmp?style=flat)](https://github.com/gisma/uavRmp)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
<!-- badges: end -->


The [uavRmp](https://github.com/gisma/uavRmp) package is designed for a light weighted  uav autonomous mission planning including full documentation capabilities. In the first place it is a simple and open source planning tool for monitoring flights of low budget drones based on ```R```. It provide an easy workflow for planning autonomous 
surveys including battery-dependent task splitting, save departures, and approaches of each monitoring chunks. 

## Introduction

The majority of the open UAV community is using the PixHawk autopilot unit and for planning probably the [MissionPlanner](https://ardupilot.org/planner/) or a bit more basic [QGroundControl](http://qgroundcontrol.com/) ground station software. Both are well documented and provide APIs (Application program interface) and easy to use GUIs (graphical user interface). Nevertheless both have only a poor planning support for terrain following autonomous flights (basically SRTM data) and no straightforward support for battery-dependent task splitting and save departures and approaches (MissionPlanner). Up to now the most affordable powerful [UgCS](https://www.ugcs.com/) software provides all of the above mentioned capabilities. However it is challenging to use especially for a fast deployment of a small local flight task as typically requested in fieldwork.

The ```uavRmp``` bridges this gap  and  generates  ```MAVLINK``` format compliant mission files that can be uploaded to the Pixhawk controller using an integrated function or externally by any Ground Control Station software.

## Supported UAV platforms

Up to now the ```uavRmp``` package has been dedicated to low budget rtf-UAVs (ready-to-fly) as the DJI series that are supported by [Litchi](https://flylitchi.com/) and Pixhawk based platforms as the Yuneec UAVs or the outdated but still running 3DR Solo. 

The core planning tool ```makeAP``` (make arial flight plan) creates either intermediate flight control files for the DJI UAVs or waypoint files for the PixHawk family. Furthermore the option `useMP` unifies flight planning for PixHawk based platforms and DJI consumer drones.  as it offers an easy conversion of surveys planned with `QgroundContro`l into the format readable by Litchi.  

## News

**NOTE:** The Litchi export for DJI Consumer drones is significantly improved now. For details have a look at the vignette.  

**NOTE:** Starting with Version 0.6.3 the `raster` package is removed and you need to provide the `terra` package `SpatRaster` format only.

**NOTE:** You may use now the survey planning tool of `QGroundControl` or `Missionplanner` and convert it either to DJI compatible Litchi format or MavLink files. Both with safe flights to the mission start and RTH as well as task splitting. You will find an simple GUI interface calling `shiny::runApp(system.file("shiny/plan2litchi/", "/app.R", package = "uavRmp"))`.

**NOTE:** The DJI control files are designed for using with the proprietary `Litchi` flight control app exchange format, while the PixHawk/3DR Solo files are using the ```MAVLINK``` common message format, that is used by the PixHawk flight controller family.




## Installation

You need GDAL to be installed.  For using some of the the 3DR Solo related functions you need to install the python libs in addition.

To install the most actual version do it from ```github```  you need to have installed the ```devtools``` package.

```S
install.packages("devtools")

devtools::install_github("gisma/uavRmp", ref = "master")
```
