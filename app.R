library(shiny)
library(odbc)
library(dbplyr)
library(tidyverse)
library(pool)
library(DT)
library(data.table)
library(shinythemes)

source("modules.R")
source("tableSpec.R")

# Define UI for application that imports calibration data from .csv and uploads to database
ui <- htmlTemplate("html-templates/calibration-main.html")
  
# Define server logic
server <- function(input, output, session) {
  
  # Get new data from uploaded files
  # TODO: Omit data that are already in the database
  calib.data <- callModule(fileImport, "import.data", table.spec)
  
  # Initialize reactiveValues object to store final reviewed/edited data
  final.data <- reactiveValues()
  
  # Clean up data from uploaded files using data manipulation functions provided in the table specification
  clean.data <- reactive({
    clean.data <- list()
    for (table in table.spec) {
      # Clean up data, if present
      if (nrow(calib.data()[[table$table.name]]) > 0) {
        clean.data[[table$table.name]] <- calib.data()[[table$table.name]] %>% table$data.manip()
      } else {
        clean.data[[table$table.name]] <- data_frame()
      }
    }
    clean.data
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
    isolate({
      callModule(dataUpload, id = "CalibrationSpCond.upload", data = final.data$CalibrationSpCond(), table.spec = table.spec$CalibrationSpCond)
      callModule(dataUpload, id = "CalibrationDO.upload", data = final.data$CalibrationDO(), table.spec = table.spec$CalibrationDO)
      callModule(dataUpload, id = "CalibrationpH.upload", data = final.data$CalibrationpH(), table.spec = table.spec$CalibrationpH)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

