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
  
  # Loop through tables in table spec and create a tab for each one with a dataViewAndEdit module
  observe({
    #browser()
    calib.data()
    tabs <- vector(mode = "list", length = length(table.spec))
    names(tabs) <- names(table.spec)
    for (table in table.spec) {
      # Make a tab to view/edit the data table
      appendTab("view.edit.tabs", tabPanel(table$display.name, dataViewAndEditUI(table$table.name)), select = TRUE)
      # Clean up data, if present
      if (nrow(calib.data()[[table$table.name]]) > 0) {
        clean.data <- calib.data()[[table$table.name]] %>% table$data.manip()
      } else {
        clean.data <- data_frame()
      }
      
      # Display data
      callModule(dataViewAndEdit, id = table$table.name, data = clean.data, col.spec = table$col.spec)
      
    }

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

