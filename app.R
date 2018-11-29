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
dropdown.wqinstr <- arrange(db.ref.wqinstr, desc(IsActive), Model) %>% collect()
dropdown.wqinstr <- setNames(dropdown.wqinstr$ID, dropdown.wqinstr$Label)

SpCond.uploads <- reactiveVal(tibble())

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
    sidebarPanel(width = 2
                 # File input module here
    ),
    
    # Show the incoming calibration data
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("SpCond",
                           h3("Uploaded data"),
                           dataTableOutput("SpCond.in"),
                           dateInput("SpCond.date.edit", "Calibration date",
                                     value = ""),
                           textInput("SpCond.time.edit", "Calibration time"),
                           numericInput("SpCond.std.edit", "Standard (µS/cm)", value = NA),
                           numericInput("SpCond.precal.edit", "Pre-cal reading (µS/cm)", value = NA),
                           numericInput("SpCond.postcal.edit", "Post-cal reading (µS/cm)", value = NA),
                           selectInput("SpCond.instr.edit", "SpCond instrument",
                                       choices = c("", dropdown.wqinstr),
                                       selected = NA),
                           textAreaInput("SpCond.notes.edit", "Notes"),
                           actionButton("SpCond.delete", "Delete"),
                           actionButton("SpCond.cancel", "Cancel"),
                           actionButton("SpCond.save", "Save")
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
  # TODO: Omit data that is already in the database
  SpCond.uploads <- observeEvent(input$files.in, {
    data.in <- readFiles(input$files.in$datapath,
                         input$files.in$name,
                         "*_CalibrationSpCond.csv",
                         col.types = cols(CalibrationDate = col_character(),
                                          CalibrationTime = col_time(),
                                          StandardValue_microS_per_cm = col_double(),
                                          PreCalibrationReading_microS_per_cm = col_double(),
                                          PostCalibrationReading_microS_per_cm = col_double(),
                                          SpCondInstrumentGUID = col_character(),
                                          Notes = col_character(),
                                          GUID = col_character(),
                                          DateCreated = col_character()))
    if (nrow(data.in > 0)) {
      data.in <- data.in %>%
        mutate(CalibrationTime = format(CalibrationTime, "%H:%M:%S"),
               CalibrationDate = as.Date(CalibrationDate, format = "%m/%d/%Y")) %>%  # Format dates and times so they display properly
        left_join(db.ref.wqinstr, by = c("SpCondInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
        select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -SpCondInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
        rename(SpCondInstrumentID = ID)
    }
    SpCond.uploads(data.in)
  })
  
  # Get new dissolved oxygen calibration data from uploaded files

  
  # Get new pH calibration data from uploaded files

  
  # Display imported calibration data
  SpCond.dt.proxy <- dataTableProxy("SpCond.in")
  
  output$SpCond.in <- renderDT({
    input$SpCond.save
    input$SpCond.delete
    if (nrow(SpCond.uploads()) > 0) {
      SpCond.uploads() %>%
        left_join(db.ref.wqinstr, by = c("SpCondInstrumentID" = "ID"), copy = TRUE) %>%  # Join to WQ instrument table
        rename(SpCondInstrumentLabel = Label) %>%
        select(SpCondInstrumentLabel, CalibrationDate, CalibrationTime, StandardValue_microS_per_cm, PreCalibrationReading_microS_per_cm, PostCalibrationReading_microS_per_cm) %>%
        singleSelectDT(col.names = c('Instrument', 'Date', 'Time', 'Standard (µS/cm)', 'Pre (µS/cm)', 'Post (µS/cm)'))
    }
  })
  
  # Populate editable input boxes with values from the selected row
  observe({
    input$SpCond.in_rows_selected
    # TODO: Check if there are unsaved changes in the input boxes before deselecting a row or selecting a new row
    isolate({
      # If a row is selected, populate input boxes with values from that row
      if (length(input$SpCond.in_rows_selected) == 1) {
        updateDateInput(session = session, inputId = "SpCond.date.edit", value = SpCond.uploads()$CalibrationDate[input$SpCond.in_rows_selected])
        updateTextInput(session = session, inputId = "SpCond.time.edit", value = SpCond.uploads()$CalibrationTime[input$SpCond.in_rows_selected])
        updateNumericInput(session = session, inputId = "SpCond.std.edit", value = SpCond.uploads()$StandardValue_microS_per_cm[input$SpCond.in_rows_selected])
        updateNumericInput(session = session, inputId = "SpCond.precal.edit", value = SpCond.uploads()$PreCalibrationReading_microS_per_cm[input$SpCond.in_rows_selected])
        updateNumericInput(session = session, inputId = "SpCond.postcal.edit", value = SpCond.uploads()$PostCalibrationReading_microS_per_cm[input$SpCond.in_rows_selected])
        updateSelectInput(session = session, inputId = "SpCond.instr.edit", selected = SpCond.uploads()$SpCondInstrumentID[input$SpCond.in_rows_selected])
        updateTextAreaInput(session = session, inputId = "SpCond.notes.edit", value = SpCond.uploads()$Notes[input$SpCond.in_rows_selected])
        
      # If no rows are selected, clear input boxes
      } else {
        updateDateInput(session = session, inputId = "SpCond.date.edit", value = NA)
        updateTextInput(session = session, inputId = "SpCond.time.edit", value = "")
        updateNumericInput(session = session, inputId = "SpCond.std.edit", value = NA)
        updateNumericInput(session = session, inputId = "SpCond.precal.edit", value = NA)
        updateNumericInput(session = session, inputId = "SpCond.postcal.edit", value = NA)
        updateSelectInput(session = session, inputId = "SpCond.instr.edit", selected = "")
        updateTextAreaInput(session = session, inputId = "SpCond.notes.edit", value = "")
      }
    })
  })
  
  # Save changes to SpCond data
  observeEvent(input$SpCond.save, {
    # Save the row number that was selected
    selected.row <- input$SpCond.in_rows_selected
    
    # Get the new values from the input boxes and coerce them to the correct data types
    new.data <- tibble(CalibrationDate =  input$SpCond.date.edit,
                       CalibrationTime = input$SpCond.time.edit,
                       StandardValue_microS_per_cm = as.numeric(input$SpCond.std.edit),
                       PreCalibrationReading_microS_per_cm = as.numeric(input$SpCond.precal.edit),
                       PostCalibrationReading_microS_per_cm = as.numeric(input$SpCond.postcal.edit),
                       SpCondInstrumentID = as.integer(input$SpCond.instr.edit),
                       Notes = input$SpCond.notes.edit)
    
    #Assign the new values to the SpCond data frame
    new.SpCond <- SpCond.uploads()
    new.SpCond[input$SpCond.in_rows_selected,
               c("CalibrationDate", "CalibrationTime",
                 "StandardValue_microS_per_cm", "PreCalibrationReading_microS_per_cm", "PostCalibrationReading_microS_per_cm",
                 "SpCondInstrumentID", "Notes")] <- new.data[1, ]
    SpCond.uploads(new.SpCond)
    
    # Re-select the row that was selected
    SpCond.dt.proxy %>% selectRows(selected.row)
  })
  
  # Cancel changes to SpCond data
  observeEvent(input$SpCond.cancel, {
    SpCond.dt.proxy %>% selectRows(NULL)
  })
  
  # Delete a row of SpCond data
  
  observeEvent(input$SpCond.delete, {
    # If no rows are selected, don't do anything
    if (!is.null(input$SpCond.in_rows_selected)) {
      # Save the row number that was selected
      selected.row <- input$SpCond.in_rows_selected
      # Prompt user to confirm deletion
      showModal({
        modalDialog(
          h3("Confirm deletion"),
          p("Are you sure that you want to delete the selected row of data?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("SpCond.conf.delete", "Delete")
          ),
          easyClose = FALSE,
          size = "m"
        )
      })
      # Re-select the row that was selected for deletion (the modal dialog will otherwise clear row selections)
      SpCond.dt.proxy %>% selectRows(selected.row)
    }
    
  })
  
  observeEvent(input$SpCond.conf.delete, {
    # Delete the selected row
    new.SpCond <- SpCond.uploads()
    new.SpCond <- new.SpCond[-input$SpCond.in_rows_selected, ]
    SpCond.uploads(new.SpCond)
    
    removeModal()
  })
  
  output$DO.in <- renderDT(singleSelectDT(DO.uploads()))
  output$pH.in <- renderDT(singleSelectDT(pH.uploads()))
}

# Run the application 
shinyApp(ui = ui, server = server)

