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
#data <- read.csv("data/data.csv")
#map <- readOGR("data/fe_2007_39_county/fe_2007_39_county.shp")

# ui object
ui <- fluidPage(

  titlePanel(p("QGC Survey to Litchi Converter", style = "color:#3474A7")),
  includeMarkdown("home.md"),
  fluidRow(
    column(2, wellPanel(
      fileInput("planfile", "choose flighplan", multiple = FALSE,
                accept = c(
                  ".waypoints",
                  ".plan")),
      fileInput("demfile", "choose dem", multiple = FALSE,
                accept = c( 
                  ".tif",
                  ".asc")),
      actionButton("do", "Convert Data"),
      
    )),
      column(2, wellPanel(
  textInput("projectDir", "Provide a Project Folder name", "~/tmp"),
  verbatimTextOutput("value1"),
    numericInput("maxWayPoints", "Maximum waypoints", 9999, min = 3, max = 9999),
    verbatimTextOutput("value2"),
  numericInput("altFilter", "Provide the altFilter value in meter",  5.0, min = 0.1, max = 25),
  verbatimTextOutput("value3"),
  textInput("followSurface", "Apply surface Analysis for Flightplan (TRUE/FALSE)", "TRUE"),
  verbatimTextOutput("value4"),
  numericInput("followSurfaceRes", "Maximum waypoints", 5, min = 1, max = 100),
  verbatimTextOutput("value5"),
  numericInput("horizonFilter", "Maximum waypoints", 5, min = 1, max = 100),
  verbatimTextOutput("value6"),
  textInput("cameraType", "Camera Type (see help)", "dji32"),
  verbatimTextOutput("value7"),
      ))),
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
               maxFlightTime = input$maxWayPoints,
               altFilter = input$altFilter,
               followSurface = input$followSurface,
               cameraType = input$cameraType ,
               followSurfaceRes = input$followSurfaceRes,
               horizonFilter = input$horizonFilter,
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