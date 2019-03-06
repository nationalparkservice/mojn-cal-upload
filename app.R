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
                                                              tabsetPanel(id = "review.tabs", type = "pills",
                                                                          tabPanel("Specific Conductance", id = "spcond.tab",
                                                                                   dataViewAndEditUI("CalibrationSpCond")
                                                                          ),
                                                                          tabPanel("Dissolved Oxygen", id = "do.tab",
                                                                                   dataViewAndEditUI("CalibrationDO")
                                                                          ),
                                                                          tabPanel("pH", id = "ph.tab",
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
                                           disabled(tags$li(id = "next.review", class = "next",
                                                   tags$a(id = "btn.next.review", href = "#", "Review Data"))),
                                           hidden(
                                             tags$li(id = "next.upload", class = "next",
                                                     tags$a(id = "btn.next.upload", href = "#", "Upload Data"))),
                                           hidden(
                                             tags$li(id = "next.done", class = "next",
                                                     tags$a(id = "btn.next.done", href = "#", "Done"))),
                                           hidden(
                                             tags$li(id = "back.import", class = "previous",
                                                     tags$a(id = "btn.back.import", href = "#", "Back to Import"))),
                                           hidden(
                                             tags$li(id = "back.review", class = "previous",
                                                     tags$a(id = "btn.back.review", href = "#", "Back to Data Review"))),
                                           hidden(
                                             tags$li(id = "back.startover", class = "previous",
                                                     tags$a(id = "btn.back.startover", href = "#", "Import More Data")))
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
  
  # Get new data from imported files
  calib.data <- callModule(fileImport, "import.data", table.spec)
  
  # Disable next button if no data imported
  observeEvent(calib.data(), {
    
    data.exist <- FALSE
    tabs.to.show <- c()
    
    # Check if data were imported
    if (nrow(calib.data()$CalibrationSpCond) > 0) {
      tabs.to.show <- c(tabs.to.show, "Specific Conductance")
      data.exist <- TRUE
    }
    
    if (nrow(calib.data()$CalibrationDO) > 0) {
      tabs.to.show <- c(tabs.to.show, "Dissolved Oxygen")
      data.exist <- TRUE
    }
    
    if (nrow(calib.data()$CalibrationpH) > 0) {
      tabs.to.show <- c(tabs.to.show, "pH")
      data.exist <- TRUE
    }
    
    tabs.to.hide <- setdiff(c("Specific Conductance", "Dissolved Oxygen", "pH"), tabs.to.show)
    
    if (data.exist) {
      # Enable next button if data were imported
      shinyjs::enable("next.review")
      # Advance from import screen to review screen on next button click
      onclick("btn.next.review", {
        shinyjs::hide("next.review")
        shinyjs::hide("import.card")
        shinyjs::show("review.card")
        shinyjs::show("next.upload")
        shinyjs::show("back.import")
        for (tab in tabs.to.show) {
          showTab("review.tabs", tab)
        }
        for (tab in tabs.to.hide) {
          hideTab("review.tabs", tab)
        }
      })
    } else {
      # Disable next button if no data imported
      shinyjs::disable("next.review")
      # Do nothing when disabled button is clicked
      onclick("btn.next.review", {})
    }
  })
  
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
  upload.success <- callModule(dataUpload, id = "data.upload", data = final.data, upload.function = function(data){
    table.spec$CalibrationSpCond$data.upload(isolate(data$CalibrationSpCond()))
    table.spec$CalibrationDO$data.upload(isolate(data$CalibrationDO()))
    table.spec$CalibrationpH$data.upload(isolate(data$CalibrationpH()))
  })
  
  # Handle events
  
  observeEvent(upload.success(), {
    if (upload.success()) {
      # Show "done" and "import more" buttons
      shinyjs::show("next.done")
      shinyjs::show("back.startover")
      shinyjs::hide("back.review")
    }
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
  
  # Go back to import screen after data have been uploaded to database
  onclick("btn.back.startover", {
    shinyjs::hide("upload.card")
    shinyjs::hide("back.startover")
    shinyjs::hide("next.done")
    shinyjs::show("next.review")
    shinyjs::show("import.card")
  })
  
  # Go back to home screen after data have been uploaded to database
  onclick("btn.next.done", {
    shinyjs::hide("upload.card")
    shinyjs::hide("back.startover")
    shinyjs::hide("next.done")
    # TODO: When data browser (or home screen with instructions?) is implemented, go there instead
    shinyjs::show("next.review")
    shinyjs::show("import.card")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

