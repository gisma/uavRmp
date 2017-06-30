# uavRmp
Unmanned Aerial Vehicle R based Mission Planning


The [uavRmp](https://github.com/gisma/uavRmp) package for is designed 
for uav autonomous flight planning focussing the opportunity for low 
budget, open source based mission planning of monitoring and single 
position flights using ```R```. It is aiming to provide an easy to use 
workflow for controlling rtf UAVs from planning and flying autonomous 
surveys. It belongs to the ```uavR``` package family that provides more 
tools for pre- and post-processing of the derived data.

## Supported UAV platforms

Up to now it is dedicated to low budget rtf-UAVs as the DJI Phantom series and all Pixhawk based uavs like the 3DR Solo.

The reason using DJI is their absolute straightforward usage. Everybody can fly with a DJI but the price to pay off is a hermetically closed system. Only the litchi app provides additionally to a cloud based mission planer an offline/standalone interface that is up to date and facilitate the upload of a CSV formatted waypoint file to control autonomous flights with the the Phantom.

The open uav community is focused on the PixHawk autopilot unit and the Mission Planner software. It is well documented and several APIs are provided. Nevertheless an affordable terrain following autonomous flight planning tool is not available yet. It creates basic ```MAVLINK``` format compliant mission files that can be uploaded directly or via a Ground Control Station to the Pixhawk controller/Solo.

## Mission planning 

The core planning tool ```makeFP``` (make flight plan) creates either intermediate flight control files for the dji phantom x UAVs or ready to upload control files for the 3DR Solo. The dji control files are designed for using with the proprietary litchi flight control app exchange format, while the 3DR Solo files are using the ```MAVLINK``` common message format, that is used by the PixHawk flight controller family.

## Analysis

The package family consists of 4 parts:

  * flight planning ```uavRmp```
  * forest analysis ```uavRfa```
  * remote sensing ```uavRrs```
  * archaeology ```uavRao```
  

Please note that uavRmp is making strong use of CLI tools like GDAL. The setup  of the correct linkage to these APIs can be cumbersome. For using the ```uavRmp``` package you need to install the  ```link2GI``` package. Because the CRAN version is a bit outdated you should get the actual github hosted version of the [link2GI](https://github.com/gisma/link2GI/blob/master/README.md) package. 

Nevertheless all mentioned software packages have to be installed correctly on your the OS. It is just in parts tested under Windows but should run....The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. If interested in setting up a clean Xubuntu or Mint Linux and then  use the  [postinstall script](http://giswerk.org/doku.php?do=export_code&id=tutorials:softgis:xubuntu:xubuntugis&codeblock=0setup) for installing most of the stuff. For using some of the the Solo related functions you need to install the [dronekit](http://python.dronekit.io/develop/installation.html) python libs in addition.

A full list of necessary libaries and binaries beyond ```R``` will soon be provided.


To install from ```github```  you need to have installed the ```devtools``` package.

```S
devtools::install_github("gisma/uavRmp", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/uavRmp", ref = "master", dependencies = TRUE)
```
