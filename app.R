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
      tabsetPanel(type = "tabs",
                  tabPanel("SpCond",
                           dataViewAndEditUI("SpCond")
                  ),
                  tabPanel("DO",
                           h3("Uploaded data"),
                           dataTableOutput("DO.in")
                  ),
                  tabPanel("pH",
                           h3("Uploaded data"),
                           dataTableOutput("pH.in")
                  )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Get new specific conductance calibration data from uploaded files
  # TODO: Omit data that are already in the database
  calib.data <- callModule(fileImport, "import.data", calib.table.spec)
  
  sp.cond <- reactive({
    calib.data()$CalibrationSpCond %>%
      mutate(CalibrationTime = format(CalibrationTime, "%H:%M"),
             CalibrationDate = as.Date(CalibrationDate, format = "%m/%d/%Y")) %>%  # Format dates and times so they display properly
      left_join(db.ref.wqinstr, by = c("SpCondInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table by GUID
      select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -SpCondInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
      rename(SpCondInstrumentID = ID)
  })

  # Get new dissolved oxygen calibration data from uploaded files

  
  # Get new pH calibration data from uploaded files
  
  # Display SpCond data table and edit boxes
  callModule(dataViewAndEdit, "SpCond", data = sp.cond(), col.spec = SpCond.col.spec)
}

# Run the application 
shinyApp(ui = ui, server = server)

