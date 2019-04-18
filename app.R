rm(list = ls())
gc()

library(shiny)
library(shinyjs)
library(odbc)
library(dbplyr)
library(tidyverse)
library(DT)
library(data.table)
library(shinythemes)

source("tableSpec.R")
source("modules.R")

# Define UI for application that imports calibration data from .csv and uploads to database
ui <- tagList(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  navbarPage("Water Quality Calibration Data",
             tabPanel("Upload",
                      fluidPage(
                        fluidRow(
                          column(10, offset = 1,
                                 # Data import card
                                 tags$div(id = "import-card", class = "panel panel-default ",
                                          tags$div(class = "panel-body",
                                                   fluidRow(
                                                     column(2),
                                                     column(8, align = "center",
                                                            h4("Import Summary", id = "import.summary"),
                                                            dataTableOutput("data.imported")
                                                     ),
                                                     column(2)
                                                   )
                                          )
                                 ),
                                 # Data review and edit card
                                 hidden(
                                   tags$div(id = "review-card", class = "panel panel-default ",
                                            tags$div(class = "panel-body",
                                                     fluidRow(
                                                       column(12,
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
                                   tags$div(id = "upload-card", class = "panel panel-default ",
                                            tags$div(class = "panel-body",
                                                     fluidRow(
                                                       column(12, align = "center",
                                                              h2(id = "submit-header", "Submit data to the database"),
                                                              p(id = "submit-instructions", "You are about to submit water quality calibration data. Before you click submit, make sure that you have thoroughly reviewed all of the data."),
                                                              hidden(h2(id = "submit.success.msg", "Success!")),
                                                              hidden(p(id = "submit.success.info", "Data were successfully added to the database. You may now close this browser tab.")),
                                                              actionButton("submit", class = "btn btn-lg btn-success", "Submit data")
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
             )#,
             # tabPanel("View"),
             # tabPanel("QA/QC")
  )
)

# Define server logic
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ### Import data ###
  all.data <- list()
  data.imports <- list()
  
  # Get list of imported files
  path.to.data <- "M:\\MONITORING\\WQCalibration\\DataFromFilemaker"
  data.file.names <- list.files("M:\\MONITORING\\WQCalibration\\DataFromFilemaker", pattern = ".*Calibration(SpCond|DO|pH)\\.csv")
  data.file.paths <- paste(path.to.data, data.file.names, sep = "\\")
  
  # Get data from uploaded files
  for (tbl in table.spec) {
    data.in <- readFiles(data.file.paths, search.string = tbl$search.string, col.types = tbl$col.types)
    all.data[[tbl$table.name]] <- data.in
  }
  
  # Figure out which, if any, records are already present in the database
  if (nrow(all.data$CalibrationSpCond > 0)) {
    existing.SpCond <- db.SpCond %>%
      filter(GUID %in% all.data$CalibrationSpCond$GUID) %>%
      collect() 
  } else {
    existing.SpCond <- db.SpCond %>%
      filter(GUID != GUID) %>%
      collect() 
  }
  
  if (nrow(all.data$CalibrationDO > 0)) {
    existing.DO <- db.DO %>%
      filter(GUID %in% all.data$CalibrationDO$GUID) %>%
      collect() 
  } else {
    existing.DO <- db.DO %>%
      filter(GUID != GUID) %>%
      collect() 
  }
  
  if (nrow(all.data$CalibrationpH > 0)) {
    existing.pH <- db.pH %>%
      filter(GUID %in% all.data$CalibrationpH$GUID) %>%
      collect() 
  } else {
    existing.pH <- db.pH %>%
      filter(GUID != GUID) %>%
      collect() 
  }
  
  # Omit any data already in the database
  data.imports$CalibrationSpCond <- all.data$CalibrationSpCond %>%
    filter(!(GUID %in% existing.SpCond$GUID))
  
  data.imports$CalibrationDO <- all.data$CalibrationDO %>%
    filter(!(GUID %in% existing.DO$GUID))
  
  data.imports$CalibrationpH <- all.data$CalibrationpH %>%
    filter(!(GUID %in% existing.pH$GUID))
  
  
  importStatusText <- function(count) {
    if (count == 0) {
        txt <- "No new calibration data found."
    } else {
      txt <- paste(count, "calibration(s) pending review.", sep = " ")
    }
    return(txt)
  }
  
  vImportStatusText <- Vectorize(importStatusText, "count")
  
  output$data.imported <- renderDataTable({
    
    spcond.count <- nrow(data.imports$CalibrationSpCond)
    do.count <- nrow(data.imports$CalibrationDO)
    ph.count <- nrow(data.imports$CalibrationpH)
    
    spcond.ignored.count <- nrow(existing.SpCond)
    do.ignored.count <- nrow(existing.DO)
    ph.ignored.count <- nrow(existing.pH)
    
    summary <- data_frame(metric = c("Specific Conductance", "Dissolved Oxygen", "pH"),
                          import.count = c(spcond.count, do.count, ph.count))
    
    summary <- summary %>%
      mutate(import.message = paste(vImportStatusText(import.count), 
                                    sep = " ")) %>%
      select(metric, import.message)
    
    datatable(summary,
              rownames = FALSE,
              colnames = "",
              selection = "none",
              options = list(dom = "b",
                             ordering = F,
                             columnDefs = list(list(width = '200px', targets = "_all")))) %>%
      formatStyle(1, `text-align` = 'right')
  })
  
  # Disable next button if no data imported
  data.exist <- FALSE
  tabs.to.show <- c()
  
  # Check if data were imported
  if (nrow(data.imports$CalibrationSpCond) > 0) {
    tabs.to.show <- c(tabs.to.show, "Specific Conductance")
    data.exist <- TRUE
  }
  
  if (nrow(data.imports$CalibrationDO) > 0) {
    tabs.to.show <- c(tabs.to.show, "Dissolved Oxygen")
    data.exist <- TRUE
  }
  
  if (nrow(data.imports$CalibrationpH) > 0) {
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
      shinyjs::hide("import-card")
      shinyjs::show("review-card")
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
  
  # Initialize reactiveValues object to store final reviewed/edited data
  final.data <- reactiveValues()
  
  # Clean up data from uploaded files using data manipulation functions provided in the table specification
  clean.data <- list()
  for (table in table.spec) {
    # Clean up data, if present
    if (nrow(data.imports[[table$table.name]]) > 0) {
      clean.data[[table$table.name]] <- data.imports[[table$table.name]] %>% table$data.manip()
    } else {
      clean.data[[table$table.name]] <- data_frame()
    }
  }
  
  if (length(clean.data) > 0) {
    final.data$CalibrationSpCond <- callModule(dataViewAndEdit, id = "CalibrationSpCond", data = clean.data$CalibrationSpCond, col.spec = table.spec$CalibrationSpCond$col.spec)
    final.data$CalibrationDO <- callModule(dataViewAndEdit, id = "CalibrationDO", data = clean.data$CalibrationDO, col.spec = table.spec$CalibrationDO$col.spec)
    final.data$CalibrationpH <- callModule(dataViewAndEdit, id = "CalibrationpH", data = clean.data$CalibrationpH, col.spec = table.spec$CalibrationpH$col.spec) 
    
  }
  
  
  # Data upload
  observeEvent(input$submit, {
    # Prompt user to confirm upload
    showModal({
      modalDialog(
        h3("Confirm upload"),
        p("You are about to upload data to the master database. Would you like to continue?"),
        footer = tagList(
          modalButton("No, not yet"),
          actionButton(session$ns("conf.upload"), "Yes, upload the data!")
        ),
        easyClose = FALSE,
        size = "m"
      )
    })
  })
  
  upload.success <- eventReactive(input$conf.upload, {
    # Attempt to append data to table in database
    success <- FALSE
    tryCatch({
      insertInto(isolate(final.data))
      
      # If successful, display success message
      removeModal()
      showModal({
        modalDialog(
          h3("Upload success"),
          p("Successful data upload"),
          footer = tagList(
            modalButton("Ok")
          ),
          easyClose = FALSE,
          size = "s"
        )
      })
      # Disable submit button after successful upload
      shinyjs::disable("submit")
      shinyjs::show("submit.success.msg")
      shinyjs::show("submit.success.info")
      shinyjs::hide("submit-header")
      shinyjs::hide("submit-instructions")
      success <- TRUE
    },
    error = function(c) {
      # If unsuccessful, display error message
      showModal({
        modalDialog(
          h3("Upload error"),
          p("There was an error uploading the data."),
          p(paste0("Error message: ", c)),
          footer = tagList(
            modalButton("Ok")
          ),
          easyClose = FALSE,
          size = "s"
        )
      })
    },
    warning = function(c) {
      showModal({
        modalDialog(
          h3("Upload warning"),
          p("There was an error uploading the data."),
          p(paste0("Warning message: ", c)),
          footer = tagList(
            modalButton("Ok")
          ),
          easyClose = FALSE,
          size = "s"
        )
      })
    },
    message = function(c) {
      
    })
    success
  })
  
  
  # Handle events
  observeEvent(upload.success(), {
    if (upload.success()) {
      # Prevent user from going back to review once data are uploaded
      shinyjs::hide("back.review")
    }
  })
  
  # Advance from review screen to upload screen
  onclick("btn.next.upload", {
    shinyjs::hide("next.upload")
    shinyjs::hide("back.import")
    shinyjs::hide("review-card")
    shinyjs::show("upload-card")
    shinyjs::show("back.review")
  })
  
  # Go back to review screen from upload screen
  onclick("btn.back.review", {
    shinyjs::hide("upload-card")
    shinyjs::hide("back.review")
    shinyjs::show("next.upload")
    shinyjs::show("back.import")
    shinyjs::show("review-card")
  })
  
  # Go back to import screen from review screen
  onclick("btn.back.import", {
    shinyjs::hide("review-card")
    shinyjs::hide("next.upload")
    shinyjs::hide("back.import")
    shinyjs::show("next.review")
    shinyjs::show("import-card")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

