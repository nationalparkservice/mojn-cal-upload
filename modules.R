library(shiny)
library(odbc)
library(dbplyr)
library(tidyverse)
library(pool)
library(DT)

# Functions
readFiles <- function (file.paths, file.names, search.string = "*", col.types = NULL) {
  # Given a list of .csv file paths and file names, reads data into a data frame from files whose name matches the search string.
  #
  # Args:
  #   file.paths: A list of paths to the files to be read
  #   file.names: A list of the original filenames (if using R Shiny file input, this is not the same as file.paths!)
  #   search.string: Regular expression specifying which files in file.names should be read
  #   col.types: Column specification
  # Returns:
  #   A dataframe containing the data read from the input files. Note that duplicate rows will be removed.
  
  file.paths <- file.paths[grepl(search.string, file.names)]
  
  if (length(file.paths > 0)) {
    data.in <- bind_rows(lapply(file.paths, read_csv, col_types = col.types))
    data.in <- unique(data.in)  # Get rid of any duplicate rows of data
  } else {
    data.in <- data.frame()
  }
  
  return(data.in)
}

# CSV file import module UI
# TODO: read from JSON
fileImportInput <- function(id) {
  # File import UI module.
  #
  # Args:
  #   id: The namespace for the module

  ns <- NS(id)
  
  fileInput("files.in", "Select data files to upload",
            multiple = TRUE,
            accept = ".csv")
  
}

# CSV file import module server function
fileImport <- function(input, output, session, table.spec) {
  # File import module that reads csv data files into dataframes in a reactiveValues object.
  #
  # Args:
  #   input, output, session: required parameters for Shiny server function
  #   table.spec: A list of table types (e.g. CalibrationDO, WaterQualityActivity), each a list containing the following:
  #                 search.string: Regular expression specifying which files in file.names correspond to that table type
  #                 col.types: Column specification
  #                 data.manip: Function to manipulate the data. Must take the data table as its first argument.
  #
  # Returns:
  #   A reactiveValues object containing the data read from the input files. Note that duplicate rows will be removed.
  
  # Initialize reactiveValues object
  data.imports <- reactiveValues()
  
  # When new files are uploaded, populate data.imports
  observeEvent(input$files.in, {
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
  
}








