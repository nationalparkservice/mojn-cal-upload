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
  
  observeEvent(clean.data(), {
    final.data$CalibrationSpCond <- callModule(dataViewAndEdit, id = "CalibrationSpCond", data = clean.data()$CalibrationSpCond, col.spec = table.spec$CalibrationSpCond$col.spec)
    final.data$CalibrationDO <- callModule(dataViewAndEdit, id = "CalibrationDO", data = clean.data()$CalibrationDO, col.spec = table.spec$CalibrationDO$col.spec)
    final.data$CalibrationpH <- callModule(dataViewAndEdit, id = "CalibrationpH", data = clean.data()$CalibrationpH, col.spec = table.spec$CalibrationpH$col.spec) 
  })
  
  # Call the data upload module
  callModule(dataUpload, id = "data.upload", data = final.data, upload.function = function(data){
    table.spec$CalibrationSpCond$data.upload(isolate(data$CalibrationSpCond()))
    table.spec$CalibrationDO$data.upload(isolate(data$CalibrationDO()))
    table.spec$CalibrationpH$data.upload(isolate(data$CalibrationpH()))
  })
  
  # Handle events
  
  # Advance from import screen to review screen
  onclick("btn.next.review", {
    shinyjs::hide("next.review")
    shinyjs::hide("import.card")
    shinyjs::show("review.card")
    shinyjs::show("next.upload")
    shinyjs::show("back.import")
  })
  
  # Advance from review screen to upload screen
  onclick("btn.next.upload", {
    shinyjs::hide("next.upload")
    shinyjs::hide("back.import")
    shinyjs::hide("review.card")
    shinyjs::show("upload.card")
    shinyjs::show("back.review")
  })
  
  # Go back to review screen from upload screen
  onclick("btn.back.review", {
    shinyjs::hide("upload.card")
    shinyjs::hide("back.review")
    shinyjs::show("next.upload")
    shinyjs::show("back.import")
    shinyjs::show("review.card")
  })
  
  # Go back to import screen from review screen
  onclick("btn.back.import", {
    shinyjs::hide("review.card")
    shinyjs::hide("next.upload")
    shinyjs::hide("back.import")
    shinyjs::show("next.review")
    shinyjs::show("import.card")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

