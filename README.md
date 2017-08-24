# uavRmp
Unmanned Aerial Vehicle R based Mission Planning


The [uavRmp](https://github.com/gisma/uavRmp) package is designed 
for uav autonomous mission planning. In the first place it is a simple and open source planning tool for monitoring flights of low budget drones based on ```R```. It provide an easy workflow for planning autonomous 
surveys including battery-dependent task splitting, save departures, and approaches of each monitoring chunks. It belongs to the ```uavR``` package family that provides more functionality for the pre- and post-processing as well as the analysis of the derived data.

## Supported UAV platforms

Up to now it has been dedicated to low budget rtf-UAVs (ready-to-fly-UAVs) as the DJI Phantom series and the 3DR Solo. However the current and future support will cover all Pixhawk based UAVs.

The open UAV community is focused on the PixHawk autopilot unit and the [MissionPlanner](http://ardupilot.org/planner/) or [APM Planner 2](http://ardupilot.org/planner2/) software. Both are well documented and provide APIs (Application program interface) and easy to use GUIs (graphical user interface). Nevertheless they are so far missing either planning capability (APM Planner) or a terrain following autonomous flight planning tool, that is also dealing with battery-dependent task splitting and save departures and approaches (MissionPlanner). Other groundstation software solutions like the powerful [UgCS](https://www.ugcs.com/) are supporting custom DEM/DSM (in the commercial Pro license) but are still lacking an adequate capability for dealing with highly complex surfaces combined with low above surface flight altitudes.

The ```uavRmd``` bridges this gap  and  generates  ```MAVLINK``` format compliant mission files that can be uploaded to the Pixhawk controller using an integrated function or externally by any Ground Control Station software.

The reason using DJI is their absolute straightforward usage. Everybody can fly with a DJI but the price to pay is a hermetically closed system. Only the litchi app provides additionally to a cloud based mission planner an offline/standalone interface that is up to date and facilitates the upload of a CSV formatted waypoint file to control autonomous flights with the the Phantom.


## Mission planning 

The core planning tool ```makeFP``` (make flight plan) creates either intermediate flight control files for the dji phantom x UAVs or ready to upload control files for the 3DR Solo. The dji control files are designed for using with the proprietary [Litchi](https://flylitchi.com/) flight control app exchange format, while the 3DR Solo files are using the ```MAVLINK``` common message format, that is used by the PixHawk flight controller family.

## The family

The package family consists of 4 parts:

  * flight planning ```uavRmp```
  * forest analysis ```uavRfa```
  * remote sensing ```uavRrs```
  * archaeology ```uavRao```
  

Please note that uavRmp is making strong use of CLI tools like GDAL. The setup  of the correct linkage to these APIs can be cumbersome. For using the ```uavRmp``` package you need to install the  ```link2GI``` package. Because the CRAN version is a bit outdated you should get the actual github hosted version of the [link2GI](https://github.com/gisma/link2GI/blob/master/README.md) package. 

Nevertheless, all mentioned software packages have to be installed correctly on your OS. It is just in parts tested under Windows but should run....The easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. If interested in setting up a clean Xubuntu or Mint Linux you can use the  [postinstall script](http://giswerk.org/doku.php?do=export_code&id=tutorials:softgis:xubuntu:xubuntugis&codeblock=0setup) for installing most of the stuff. For using some of the the Solo related functions you need to install the [dronekit](http://python.dronekit.io/develop/installation.html) python libs in addition.

A full list of necessary libraries and binaries beyond ```R``` will soon be provided.


To install from ```github```  you need to have installed the ```devtools``` package.

```S
devtools::install_github("gisma/uavRmp", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/uavRmp", ref = "master", dependencies = TRUE)
```
