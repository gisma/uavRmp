library(uavRmp)
fp = makeAP(projectDir = "~/Desktop/Schreibtisch/forgenius/",
            surveyArea="~/Desktop/Schreibtisch/forgenius/lacanau_all_yuneec.plan",
            useMP = TRUE,
            demFn = "~/Desktop/Schreibtisch/forgenius/lacanau-dem.tif",
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
fp = makeAP(projectDir = "~/Desktop/tmp",
            surveyArea="~/Desktop/tmp/test.plan",
            followSurface = TRUE,
            useMP = TRUE,
            demFn = demFn ,
            altFilter = 1,
            
           cameraType ="dji32",
           uavType = "dji_csv")    

demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey30m.plan", package = "uavRmp")
fp <- makeAP(projectDir = "~/Desktop/tmp",
             surveyArea=tutorial_flightArea, #"~/Desktop/tmp/DJI_2_06gsd_OFM_grass.plan",
             useMP = TRUE,
             altFilter = 5,
             followSurface = TRUE,
             demFn = demFn, #"~/Desktop/tmp/DSM_MOF_4326.tif",

             cameraType ="dji32",followSurfaceRes = 3,horizonFilter = 3,
             uavType = "dji_csv", 
             above_ground = FALSE)
