library(uavRmp)
fp = makeAP(projectDir = "/media/creu/meta/forgenius-pn/",
            surveyArea="/media/creu/meta/forgenius-pn/test04.plan",
            useMP = T,
            demFn = "/media/creu/meta/forgenius-pn/dsm.tif",
            noFiles = 1,flightAltitude = 15,
            followSurface = T,followSurfaceRes = 3,horizonFilter = 3,
            altFilter =0.9,
            cameraType ="dji32",
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
            useMP = TRUE,
           noFiles=1,
           cameraType ="dji4k",
           uavType = "dji_csv")    

demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey.plan", package = "uavRmp")
fp <- makeAP(projectDir = "~/Desktop/tmp",
             surveyArea="~/Desktop/tmp/DJI_2_06gsd_OFM_grass.plan",
             useMP = TRUE,
             noFiles = 1,
             followSurface = TRUE,
             demFn = "~/Desktop/tmp/DSM_MOF_4326.tif",

             cameraType ="YUN90",
             uavType = "pixhawk", 
             above_ground = FALSE)

demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey30m.plan", package = "uavRmp")
fp <- makeAP(projectDir = tempdir(),
             surveyArea = tutorial_flightArea,
             useMP = TRUE,
             followSurfaceRes = 2, horizonFilter = 2, altFilter = 5,
             
             followSurface = TRUE,
             above_ground = FALSE,
             demFn = demFn,
             uavType = "dji_csv" 
)

