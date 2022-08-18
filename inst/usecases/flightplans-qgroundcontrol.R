library(uavRmp)
fp = makeAP(projectDir = "~/Schreibtisch/forgenius/",
            surveyArea="~/Schreibtisch/MOF/test-altum.plan",
            useMP = TRUE,
            demFn = "~/Schreibtisch/MOF/mr-dem.tif",
           noFiles = 3,
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 35,
            altFilter = 0.5,
            cameraType ="dji4k",
            uavType = "dji_csv")    

library(uavRmp)
fp = makeAP(projectDir = "~/Schreibtisch/forgenius/",
            surveyArea="~/Schreibtisch/MOF/test-altum.plan",
            useMP = TRUE,
            demFn = "~/Schreibtisch/MOF/mr-dem.tif",
           noFiles=5,
            maxFlightTime = 10,
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 35,
            altFilter = 0.5,
            cameraType ="MAPIR2",
            uavType = "pixhawk")    