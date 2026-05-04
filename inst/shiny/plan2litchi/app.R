library(shiny)
library(uavRmp)

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- fluidPage(
  titlePanel(p("QGC Survey to Litchi Converter", style = "color:#3474A7")),
  p("Convert a QGroundControl survey .plan file to DJI compatible Litchi CSV files."),
  p("The app runs makeAP(useMP = TRUE). Flight altitude, overlap and footprint settings are read from the uploaded survey plan."),
  fluidRow(
    column(
      4,
      wellPanel(
        textInput("projectDir", "Project directory", "~/tmp"),
        fileInput("planfile",
                  "QGroundControl survey plan",
                  multiple = FALSE,
                  accept = c(".plan")),
        fileInput("demfile",
                  "DGM/DEM",
                  multiple = FALSE,
                  accept = c(".tif", ".tiff", ".asc")),
        actionButton("run", "Create Litchi files")
      )
    ),
    column(
      8,
      verbatimTextOutput("console")
    )
  )
)

server <- function(input, output, session) {
  consoleText <- reactiveVal("")
  output$console <- renderText(consoleText())

  observeEvent(input$run, {
    req(input$projectDir)
    req(input$planfile)
    req(input$demfile)

    consoleText("Running makeAP with useMP = TRUE ...")
    txt <- tryCatch(capture.output({
      makeAP(projectDir = path.expand(input$projectDir),
             surveyArea = input$planfile$datapath,
             useMP = TRUE,
             demFn = input$demfile$datapath,
             cameraType = "dji43",
             uavType = "dji_csv")
    }, type = "output"),
    error = function(e) paste("ERROR:", conditionMessage(e)))

    consoleText(paste(txt, collapse = "\n"))
  })
}

shinyApp(ui = ui, server = server)
