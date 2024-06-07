## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#   useMP = TRUE

## ----eval=FALSE---------------------------------------------------------------
#  demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#  tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey30m.plan", package = "uavRmp")
#  fp <- makeAP(projectDir = tempdir(),
#               surveyArea = tutorial_flightArea,
#               useMP = TRUE,
#               above_ground = FALSE,
#               demFn = demFn,
#               uavType = "dji_csv"
#               )

## ----eval=FALSE---------------------------------------------------------------
#  demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#  tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey30m.plan", package = "uavRmp")
#  fp <- makeAP(projectDir = tempdir(),
#               surveyArea = tutorial_flightArea,
#               useMP = TRUE,
#               buf_mult = 3,
#               above_ground = FALSE,
#               demFn = demFn,
#               uavType = "dji_csv"
#               )

## ----eval=FALSE---------------------------------------------------------------
#  demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
#  tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey30m.plan", package = "uavRmp")
#  fp <- makeAP(projectDir = tempdir(),
#               surveyArea = tutorial_flightArea,
#               useMP = TRUE,
#               buf_mult = 3,
#               followSurfaceRes = 2,
#               horizonFilter = 2,
#               altFilter = 5,
#               above_ground = FALSE,
#               demFn = demFn,
#               uavType = "dji_csv"
#               )

