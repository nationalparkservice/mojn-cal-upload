library(shiny)
library(odbc)
library(dbplyr)
library(tidyverse)
library(pool)
library(DT)

# DS.dsn <- 'driver={SQL Server Native Client 11.0};server=INPLAKE36792JNX\\SARAH_LOCAL;database=Testing_MOJN_DS_Water;trusted_connection=Yes;applicationintent=readonly'
# Database connection
pool <- dbPool(drv = odbc::odbc(),
               Driver = "SQL Server Native Client 11.0",
               Server = "INPLAKE36792JNX\\SARAH_LOCAL",
               Database = "MOJN_SharedTables",
               Trusted_Connection = "Yes")

onStop(function() {
  poolClose(pool)
})

# Load table pointers to calibration data and refs:
db.SpCond <- tbl(pool, in_schema("data", "CalibrationSpCond"))
db.DO <- tbl(pool, in_schema("data", "CalibrationDO"))
db.pH <- tbl(pool, in_schema("data", "CalibrationpH"))
db.ref.wqinstr <- tbl(pool, in_schema("ref", "WaterQualityInstrument"))

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

singleSelectDT <- function (data, col.names) {
  # Creates a DT datatable with single-row selection enabled
  #
  # Args:
  #   data: A dataframe
  #
  # Returns:
  #   A DT datatable with single-row selection enabled
  
    datatable(data, selection = list(
      mode = "single",
      target = "row"),
      colnames = col.names
    )
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
                           dataTableOutput("SpCond.in")
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
  SpCond.uploads <- reactive({
    data.in <- readFiles(input$files.in$datapath, input$files.in$name, "*_CalibrationSpCond.csv")
    if (nrow(data.in > 0)) {
      data.in <- data.in %>%
        mutate(CalibrationTime = format(CalibrationTime, "%H:%M:%S")) %>%  # Format times so they display properly
        left_join(db.ref.wqinstr, by = c("SpCondInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
        select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive) %>%  # Get rid of unnecessary columns
        rename(SpCondInstrumentID = ID, SpCondInstrumentLabel = Label)
    }
    data.in
  })
  
  # Get new dissolved oxygen calibration data from uploaded files
  DO.uploads <- reactive({
    data.in <- readFiles(input$files.in$datapath, input$files.in$name, "*_CalibrationDO.csv")
    if (nrow(data.in > 0)) {
      # Clean up incoming data and filter out rows that are already in the database and haven't been modified
      data.in <- data.in %>% 
        mutate(CalibrationTime = format(CalibrationTime, "%H:%M:%S")) %>%  # Format times so they display properly
        left_join(db.ref.wqinstr, by = c("DOInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
        select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive) %>%  # Get rid of unnecessary columns
        rename(DOInstrumentID = ID, DOInstrumentLabel = Label)
    }
    data.in
  })
  
  # Get new pH calibration data from uploaded files
  pH.uploads <- reactive({
    data.in <- readFiles(input$files.in$datapath, input$files.in$name, "*_CalibrationpH.csv")
    if (nrow(data.in > 0)) {
      data.in <- data.in %>%
        mutate(CalibrationTime = format(CalibrationTime, "%H:%M:%S")) %>%  # Format times so they display properly
        left_join(db.ref.wqinstr, by = c("pHInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
        select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive) %>%  # Get rid of unnecessary columns
        rename(pHInstrumentID = ID, pHInstrumentLabel = Label)
    }
    data.in
  })
  
  # Render new calibration data from input csv's and existing calibration data from the database as datatables
  output$SpCond.in <- renderDT({
    if (nrow(SpCond.uploads()) > 0) {
      SpCond.uploads() %>%
        select(SpCondInstrumentLabel, CalibrationDate, CalibrationTime, PreCalibrationReading_microS_per_cm, PostCalibrationReading_microS_per_cm) %>%
        singleSelectDT(col.names = c('Instrument', 'Date', 'Time', 'Pre (µS/cm)', 'Post (µS/cm)'))
    }
  })

  
  output$DO.in <- renderDT(singleSelectDT(DO.uploads()))
  output$pH.in <- renderDT(singleSelectDT(pH.uploads()))
}

# Run the application 
shinyApp(ui = ui, server = server)

