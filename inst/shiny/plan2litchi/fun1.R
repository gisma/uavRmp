makeAP(projectDir = input$projectDir,
       surveyArea=file1$datapath,
       useMP = TRUE,
       demFn = file2$datapath,
       maxFlightTime = input$maxFlightTime,
       uavType = "dji_csv") 