library(shiny)
library(odbc)
library(tidyverse)

# Functions
readFiles <- function (file.paths, file.names, search.string = "*") {
  # Given a list of .csv file paths and file names, reads data into a data frame from files whose name matches the search string.
  #
  # Args:
  #   file.paths: A list of paths to the files to be read
  #   file.names: A list of the original filenames (if using R Shiny file input, this is not the same as file.paths!)
  #   search.string: Regular expression specifying which files in file.names should be read
  #
  # Returns:
  #   A dataframe containing the data read from the input files. Note that duplicate rows will be removed.
  
  file.paths <- file.paths[grepl(search.string, file.names)]
  
  if (length(file.paths > 0)) {
    data.in <- bind_rows(lapply(file.paths, read_csv))
    data.in <- unique(data.in)  # Get rid of any duplicate rows of data
  } else {
    data.in <- data.frame()
  }
  
  return(data.in)
}

# Define UI for application that imports calibration data from .csv and uploads to database
ui <- fluidPage(
  
  # Application title
  titlePanel("Calibration data upload"),
  
  # Sidebar with input for uploading .csv files 
  sidebarLayout(
    sidebarPanel(width = 2,
                 fileInput("files.in", "Select data files to upload",
                           multiple = TRUE,
                           accept = ".csv")
    ),
    
    # Show the incoming calibration data
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("SpCond",
                           h3("Uploaded data"),
                           tableOutput("SpCond.in")
                  ),
                  tabPanel("DO",
                           h3("Uploaded data"),
                           tableOutput("DO.in")
                  ),
                  tabPanel("pH",
                           h3("Uploaded data"),
                           tableOutput("pH.in")
                  )
      )
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Get any dissolved oxygen calibration data from uploaded files
  DO.uploads <- reactive({
    data.in <- readFiles(input$files.in$datapath, input$files.in$name, "*_CalibrationDO.csv")
    if (nrow(data.in > 0)) {
      data.in$CalibrationTime <- format(data.in$CalibrationTime, "%H:%M:%S")  # Format times so they display properly
    }
    data.in
  })
  
  # Get any specific conductance calibration data from uploaded files
  SpCond.uploads <- reactive({
    data.in <- readFiles(input$files.in$datapath, input$files.in$name, "*_CalibrationSpCond.csv")
    if (nrow(data.in > 0)) {
      data.in$CalibrationTime <- format(data.in$CalibrationTime, "%H:%M:%S")  # Format times so they display properly
    }
    data.in
  })
  
  # Get any pH calibration data from uploaded files
  pH.uploads <- reactive({
    data.in <- readFiles(input$files.in$datapath, input$files.in$name, "*_CalibrationpH.csv")
    if (nrow(data.in > 0)) {
      data.in$CalibrationTime <- format(data.in$CalibrationTime, "%H:%M:%S")  # Format times so they display properly
    }
    data.in
  })
  
  output$SpCond.in <- renderTable(SpCond.uploads())
  output$DO.in <- renderTable(DO.uploads())
  output$pH.in <- renderTable(pH.uploads())
}

# Run the application 
shinyApp(ui = ui, server = server)

