This is a straightforward interface to convert a `QGroundControl` survey `.plan` file to the DJI compatible Litchi CSV format.

The app uses the special QGroundControl workflow in `makeAP(useMP = TRUE)`. Flight altitude, overlap and footprint settings are read from the uploaded survey plan. You only need to provide a project directory, the QGroundControl plan file and a digital elevation model.

Please note that there is an upload limit of 30 MB.

According to the default path, you will find the `Litchi` control files in the folder `~/tmp/flightArea/0/fp-data/control`. 

In addition you will find more usefull stuff in this folder tree. More Information at: [uavRmp on Github](https://gisma.github.io/uavRmp/)
