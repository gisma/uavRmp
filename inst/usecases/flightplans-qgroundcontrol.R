library(uavRmp)
fp = makeAP(projectDir = "~/Schreibtisch/forgenius/",
            surveyArea="~/Schreibtisch/forgenius/lacanau_all_yuneec.plan",
            useMP = TRUE,
            demFn = "~/Schreibtisch/forgenius/lacanau-dem.tif",
            noFiles = 1,
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 70,
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
            surveyArea="~/Schreibtisch/MOF/test-buche_35.plan.plan",
            useMP = TRUE,
            demFn = "~/Schreibtisch/MOF/mr-dem.tif",
           noFiles=1,
            followSurface = T,
            horizonFilter = 1,
            followSurfaceRes = 1,
            flightAltitude = 35,
            altFilter = 0.5,
            cameraType ="MAPIR2",
            uavType = "pixhawk")    

demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
tutorial_flightArea <- system.file("extdata", "qgc_survey.plan", package = "uavRmp")
fp <- makeAP(surveyArea=tutorial_flightArea,
             useMP = TRUE,
             noFiles = 1,
             followSurface = TRUE,
             demFn = demFn,
             windCondition = 1,
             uavType = "dji_csv",
             followSurfaceRes = 5,
             altFilter = .75)
