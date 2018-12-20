library(shiny)
library(odbc)
library(dbplyr)
library(tidyverse)
library(pool)
library(DT)
library(data.table)

source("modules.R")
source("tableSpec.R")

# Define UI for application that imports calibration data from .csv and uploads to database
ui <- fluidPage(
  
  # Application title
  titlePanel("Calibration data upload"),
  
  # Sidebar with input for uploading .csv files 
  sidebarLayout(
    sidebarPanel(width = 2,
      # File input module
      fileImportInput("import.data")
    ),
    
    # Show the incoming calibration data
    mainPanel(
      tabsetPanel(id = "view.edit.tabs", type = "tabs")
                  # tabPanel("SpCond",
                  #          dataViewAndEditUI("SpCond")
                  # ),
                  # tabPanel("DO",
                  #          h3("Uploaded data"),
                  #          dataTableOutput("DO.in")
                  # ),
                  # tabPanel("pH",
                  #          h3("Uploaded data"),
                  #          dataTableOutput("pH.in")
                  # )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Get new specific conductance calibration data from uploaded files
  # TODO: Omit data that are already in the database
  calib.data <- callModule(fileImport, "import.data", table.spec)
  final.data <- reactiveValues()
  
  clean.data <- reactive({
    temp <- list()
    for (table in table.spec) {
      # Clean up data, if present
      if (nrow(calib.data()[[table$table.name]]) > 0) {
        temp[[table$table.name]] <- calib.data()[[table$table.name]] %>% table$data.manip()
      } else {
        temp[[table$table.name]] <- data_frame()
      }
    }
    temp
  })
  
  # For each table in the table specification, create a tab for viewing and editing data. Create a reactiveValues object from the reviewed and edited data returned by the dataViewAndEdit module.
  final.data <- lapply(table.spec, function(table){
    force(table)  # force evaluation of the table argument so that the call to the dataViewAndEdit module gets the correct table information
    appendTab("view.edit.tabs", tabPanel(table$display.name, dataViewAndEditUI(table$table.name), dataUploadUI(paste0(table$table.name, ".upload")), select = TRUE))
    reactive(callModule(dataViewAndEdit, id = table$table.name, data = clean.data()[[table$table.name]], col.spec = table$col.spec))
  })
  
  # Call the data upload module
  # TODO: Do this with a for loop
  observe({
    final.data$CalibrationSpCond
    final.data$CalibrationDO
    final.data$CalibrationpH
    callModule(dataUpload, id = "CalibrationSpCond.upload", data = final.data$CalibrationSpCond(), col.spec = table.spec$CalibrationSpCond$col.spec)
    callModule(dataUpload, id = "CalibrationDO.upload", data = final.data$CalibrationDO(), col.spec = table.spec$CalibrationDO$col.spec)
    callModule(dataUpload, id = "CalibrationpH.upload", data = final.data$CalibrationpH(), col.spec = table.spec$CalibrationpH$col.spec)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

