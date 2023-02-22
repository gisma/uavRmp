library(uavRmp)
fp = makeAP(projectDir = "~/Desktop/uav/maaike/",
            surveyArea="~/Desktop/uav/maaike/tuni-cross.plan",
            useMP = T,
            demFn = "~/Desktop/uav/maaike/tuni.tif",
            noFiles = 1,
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 40,
            altFilter = 0.5,
            cameraType ="dji4k",
            uavType = "dji_csv")    

library(uavRmp)
fp = makeAP(projectDir = "~/Schreibtisch/MOF/",
            surveyArea="~/Schreibtisch/MOF/test-buche_100.plan",
            useMP = TRUE,
            demFn = "~/Schreibtisch/MOF/mr-dem.tif",
            noFiles = 1,
            followSurface = T,
            horizonFilter = 5,
            followSurfaceRes = 5,
            flightAltitude = 100,
            altFilter = 0.5,
            cameraType ="dji4k",
            uavType = "dji_csv")    

library(uavRmp)
fp = makeAP(projectDir = "~/Schreibtisch/MOF/",
            surveyArea="~/Schreibtisch/MOF/test-buche_35.plan",
            useMP = TRUE,
            demFn = "~/Schreibtisch/MOF/mr-dem.tif",
            noFiles=1,flightPlanMode = "terrainTrack",
            
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 35,
            altFilter = "DJI4K",
            uavType = "dji_csv")    

demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey.plan", package = "uavRmp")
fp <- makeAP(surveyArea=tutorial_flightArea,
             useMP = TRUE,
             noFiles = 1,
             followSurface = TRUE,
             demFn = demFn,
             windCondition = 1,
             uavType = "dji_csv",
             followSurfaceRes = 5,
             altFilter = .75)

library(uavRmp)
fp = makeAP(projectDir = "~/Desktop/uav/jk/",
            surveyArea="~/Desktop/uav/jk/fwiese.plan",
            useMP = TRUE,
            demFn = "~/Desktop/uav/jk/franzosenwiese_dem.tif",
            noFiles=1,
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 35,
            altFilter = 0.5,
            uavType = "dji_csv")    
library(uavRmp)
fp = makeAP(projectDir = "~/Desktop/uav/jk/",
            surveyArea="~/Desktop/uav/jk/ctal_ns.plan",
            useMP = TRUE,
            demFn = "~/Desktop/uav/jk/cbertal.tif",
            noFiles=1,
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 35,
            altFilter = 0.5,
            uavType = "dji_csv")  
