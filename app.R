library(shiny)
library(shinyjs)
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
ui <- tagList(
  useShinyjs(),
  navbarPage("Water Quality Calibration Data",
             tabPanel("Upload",
                      fluidPage(
                        fluidRow(
                          column(10, offset = 1,
                                 # Data import card
                                 tags$div(id = "import.card", class = "panel panel-default ",
                                          tags$div(class = "panel-body",
                                                   fluidRow(
                                                     column(12, align = "center",
                                                            fileImportInput("import.data")
                                                     )
                                                   )
                                          )
                                 ),
                                 # Data review and edit card
                                 hidden(
                                   tags$div(id = "review.card", class = "panel panel-default ",
                                            tags$div(class = "panel-body",
                                                     fluidRow(
                                                       column(10, offset = 1,
                                                              tabsetPanel(type = "pills",
                                                                          tabPanel("SpCond",
                                                                                   dataViewAndEditUI("CalibrationSpCond")
                                                                          ),
                                                                          tabPanel("DO",
                                                                                   dataViewAndEditUI("CalibrationDO")
                                                                          ),
                                                                          tabPanel("pH",
                                                                                   dataViewAndEditUI("CalibrationpH")
                                                                          )
                                                              )
                                                       )
                                                     )
                                            )
                                   )
                                 ),
                                 # Data upload card
                                 hidden(
                                   tags$div(id = "upload.card", class = "panel panel-default ",
                                            tags$div(class = "panel-body",
                                                     fluidRow(
                                                       column(12, align = "center",
                                                              dataUploadUI("data.upload")
                                                       )
                                                     )
                                            )
                                   )
                                 ),
                                 # Pager buttons for toggling between cards
                                 tags$nav(
                                   tags$ul(class = "pager",
                                           tags$li(id = "next.review", class = "next",
                                                   tags$a(id = "btn.next.review", href = "#", "Review Data")),
                                           hidden(
                                             tags$li(id = "next.upload", class = "next",
                                                     tags$a(id = "btn.next.upload", href = "#", "Upload Data"))),
                                           hidden(
                                             tags$li(id = "back.import", class = "previous",
                                                     tags$a(id = "btn.back.import", href = "#", "Back to Import"))),
                                           hidden(
                                             tags$li(id = "back.review", class = "previous",
                                                     tags$a(id = "btn.back.review", href = "#", "Back to Data Review")))
                                   )
                                 )
                          )
                          
                        )
                        
                      )
             ),
             tabPanel("View"),
             tabPanel("QA/QC")
  )
)

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
  
  # # For each table in the table specification, create a tab for viewing and editing data. Create a reactiveValues object from the reviewed and edited data returned by the dataViewAndEdit module.
  # final.data <- lapply(table.spec, function(table){
  #   force(table)  # force evaluation of the table argument so that the call to the dataViewAndEdit module gets the correct table information
  #   appendTab("view.edit.tabs", tabPanel(table$display.name, dataViewAndEditUI(table$table.name), dataUploadUI(paste0(table$table.name, ".upload")), select = TRUE))
  #   reactive(callModule(dataViewAndEdit, id = table$table.name, data = clean.data()[[table$table.name]], col.spec = table$col.spec))
  # })
  # 
  # # Call the data upload module
  # # TODO: Do this with a for loop
  # observe({
  #   final.data$CalibrationSpCond
  #   final.data$CalibrationDO
  #   final.data$CalibrationpH
  #   isolate({
  #     callModule(dataUpload, id = "CalibrationSpCond.upload", data = final.data$CalibrationSpCond(), table.spec = table.spec$CalibrationSpCond)
  #     callModule(dataUpload, id = "CalibrationDO.upload", data = final.data$CalibrationDO(), table.spec = table.spec$CalibrationDO)
  #     callModule(dataUpload, id = "CalibrationpH.upload", data = final.data$CalibrationpH(), table.spec = table.spec$CalibrationpH)
  #   })
  # })
  # Handle events
  
  # Advance from import screen to review screen
  onclick("btn.next.review", {
    hide("next.review")
    hide("import.card")
    show("review.card")
    show("next.upload")
    show("back.import")
  })
  
  # Advance from review screen to upload screen
  onclick("btn.next.upload", {
    hide("next.upload")
    hide("back.import")
    hide("review.card")
    show("upload.card")
    show("back.review")
  })
  
  # Go back to review screen from upload screen
  onclick("btn.back.review", {
    hide("upload.card")
    hide("back.review")
    show("next.upload")
    show("back.import")
    show("review.card")
  })
  
  # Go back to import screen from review screen
  onclick("btn.back.import", {
    hide("review.card")
    hide("next.upload")
    hide("back.import")
    show("next.review")
    show("import.card")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

