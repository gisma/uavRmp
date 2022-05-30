This is a very straightforward interface to utilize `uavRmd` to convert a `QGroundcontrol`  or `Missionplanner` flightplan to the DJI compatible Litchi csv format.

There is **aboslutley no error handling**.  Please make usere that you have installed the latest `uavRmp` package from the GitHub repository:

`devtools::install_github("gisma/uavRmp", ref = "master")`

Please note that you need to provide obligatory a project directory (which will be created if not exist) and digitial elevation model. There is an upload limit of 30 GByte.

According to the default path, you will find the `Litchi` control files in the folder `~/tmp/flightArea/0/fp-data/control`. 

In addition you will find more usefull stuff in this folder tree. More Information at: [uavRmp on Github](https://gisma.github.io/uavRmp/)
