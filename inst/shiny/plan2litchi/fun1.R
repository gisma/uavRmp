runPlan2Litchi <- function(projectDir, planfile, demfile) {
  makeAP(projectDir = path.expand(projectDir),
         surveyArea = planfile,
         useMP = TRUE,
         demFn = demfile,
         cameraType = "dji43",
         uavType = "dji_csv")
}
