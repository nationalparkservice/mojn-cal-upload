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
  
  fileInput(ns("files.in"), "Select data files to upload",
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
  #   A list containing the data read from the input files. Note that duplicate rows will be removed.
  
  data <- list()
  
  # Get list of imported files
  data.files <- reactive({
    # Do nothing if no files present
    validate(need(input$files.in, message = "Files needed"))
    input$files.in
  })
  
  # When new files are uploaded, populate data.imports
  data.imports <- eventReactive(data.files(), {
    for (tbl in table.spec) {
      data.in <- readFiles(data.files()$datapath,
                         data.files()$name,
                         tbl$search.string,
                         col.types = tbl$col.types)
      if (nrow(data.in > 0)) {
        data.in <- data.in %>% 
          unique()
      }
      data[[tbl$table.name]] <- data.in
    }
    data
  })
  
  return(data.imports)
  
}


# Data view and edit module UI
dataViewAndEditUI <- function(id) {
  # UI module for viewing data in a dataframe and selecting/editing rows.
  #
  # Args:
  #   id: The namespace for the module

  ns <- NS(id)
  
  tagList(
    h3("Uploaded data"),
    dataTableOutput("data.view"),  # Data table UI for viewing data and selecting a row
    uiOutput("data.edit")  # Dynamically generated edit boxes will go here
  )
  
}

# Data view and edit module server function
dataViewAndEdit <- function(input, output, session, col.spec, data.manip, ...) {
  # File import module that reads csv data files into dataframes in a reactiveValues object.
  #
  # Args:
  #   input, output, session: required parameters for Shiny server function.
  #   data: The data table to be viewed/edited.
  #   col.spec: A list of columns, each a list containing the following:
  #                 label: A readable, concise name that will be used to label table columns and edit boxes.
  #                 view: Boolean value indicating whether to show the column in the table.
  #                 edit: Boolean value indicating whether to show the column as an edit box when a row in the table is selected.
  #                 type: One of "select", "text", "notes", "numeric", "time", or "date", indicating what kind of input box to use.
  #                 lookup: For foreign key columns, a data table to use as a lookup table.
  #                 lookup.pk: If lookup table specified, the name of the primary key column of the lookup.
  #                 lookup.text: If lookup table specified, the name of the column in the lookup table that contains meaningful codes or labels.
  #    data.manip: Data manipulation function to be run prior to loading data into the table. Should take data as its first argument.
  #    ...: Additional arguments to data.manip
  #
  # Returns:
  #   A dataframe of reviewed data
  
  # Get list of columns to include in table
  
  # Get list of columns to include in edit boxes
  
  # Get list of fk columns
  
  # Data pre-processing
  data.to.review <- do.call(data.manip, c(data, list(...)))
  
  # Populate table
  
  # Display edit boxes
  
}
