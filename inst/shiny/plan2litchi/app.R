library(shiny)
library(uavRmp)

demFn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
tutorial_flightArea <- system.file("extdata", "tutdata_qgc_survey.plan", package = "uavRmp")
options(shiny.maxRequestSize=30*1024^2)
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

# ui object
ui <- fluidPage(

  titlePanel(p("QGC Survey to Litchi Converter", style = "color:#3474A7")),
  includeMarkdown("home.md"),
  fluidRow(
      column(2, wellPanel(
  textInput("projectDir", "Provide a Project Folder name", "~/tmp"),
  verbatimTextOutput("value1"),
  textInput("cameraType", "Camera Type (see help)", "dji43"),
  verbatimTextOutput("value7"),
      )),
  column(2, wellPanel(
    fileInput("planfile", "choose flighplan", multiple = FALSE,
              accept = c(
                ".waypoints",
                ".plan")),
    fileInput("demfile", "choose dem", multiple = FALSE,
              accept = c( 
                ".tif",
                ".asc")),
    
    
  ))
  ),
  

    mainPanel(


      pre(id = "console")
      
      
    )
  )


# server()
server <- function(input, output) {
  observe({
    file1 = input$planfile
    file2 = input$demfile
    if (is.null(file1) || is.null(file2)) {
      return(NULL)}
    #    data1 = read.csv(file1$datapath,header = TRUE, sep=",",skipNul = TRUE)
    #    data2 = read.csv(file2$datapath,header = TRUE, sep=",",skipNul = TRUE)
    observe({
      withConsoleRedirect("console", {
        # output$value <- renderText({ input$projectDir })
        makeAP(projectDir = input$projectDir,
               surveyArea=file1$datapath,
               useMP = TRUE,
               demFn = file2$datapath,
               cameraType = input$cameraType ,
               uavType = "dji_csv") 
        
      })
      # makeAP(surveyArea=file1$datapath,
      #                    useMP = TRUE,
      #                    demFn = file2$datapath,
      #        flightAltitude =70,
      #                    maxFlightTime = 25,
      #                    uavType = "dji_csv") 
    })
  }) 
}



# shinyApp()
shinyApp(ui = ui, server = server)