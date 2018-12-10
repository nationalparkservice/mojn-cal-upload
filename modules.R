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
    dataTableOutput(ns("data.view")),  # Data table UI for viewing data and selecting a row
    h3("Edit data"),
    uiOutput(ns("data.edit")),  # Dynamically generated edit boxes will go here
    actionButton(ns("delete"), "Delete"),
    actionButton(ns("cancel"), "Cancel"),
    actionButton(ns("save"), "Save")
  )
  
}

# Data view and edit module server function
dataViewAndEdit <- function(input, output, session, data, col.spec) {
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
  #                 lookup: For foreign key columns, a data table to use as a lookup table. Otherwise omit this argument.
  #                 lookup.pk: If lookup table specified, the name of the primary key column of the lookup. Otherwise omit this argument.
  #                 lookup.text: If lookup table specified, the name of the column in the lookup table that contains meaningful codes or labels. Otherwise omit this argument.
  #
  # Returns:
  #   A dataframe of reviewed data
  
  data.in <- reactive({
    # Do nothing if no data present
    validate(need(data, message = FALSE))
    data
  })
  
  table.cols <- tibble()
  edit.cols <- tibble()
  fk.cols <- c()

  for (col in names(col.spec)) {
    # Get list of columns to include in table
    if (col.spec[[col]]$view) {
      table.cols <- bind_rows(table.cols, c(name = col, label = col.spec[[col]]$label))
    }
    # Get list of columns to include in edit boxes
    if (col.spec[[col]]$edit) {
      edit.cols <- bind_rows(edit.cols, c(name = col, label = col.spec[[col]]$label, type = col.spec[[col]]$type))
    }
    # Get list of fk columns
    if (!is_empty(col.spec[[col]]$lookup)) {
     fk.cols <- c(fk.cols, col)
    }
  }
  
  # Rename fk columns 
  table.cols[which(table.cols$name %in% fk.cols), "name"] <- paste0(table.cols$name[which(table.cols$name %in% fk.cols)], "_lookup")
  
  # Populate table
  output$data.view <- renderDT({
    input$save
    input$delete

    for (col in fk.cols) {
      lookup.tbl <- col.spec[[col]]$lookup  # get lookup table
      lookup.pk <- col.spec[[col]]$lookup.pk  # primary key of lookup table
      lookup.text <- col.spec[[col]]$lookup.text  # column in lookup table to display
      
      # Join data table to lookup table
      data.view <- data.in() %>%
        left_join(lookup.tbl, by = setNames(lookup.pk, col))
      
      # 
      data.view <- data.view %>%
        setnames(old = lookup.text, new = paste0(col, "_lookup"))# %>%
        #select(-lookup.text)
    }

    data.view %>%
      select(table.cols$name) %>%
      singleSelectDT(col.names = table.cols$label)

  })
  
  makeEditBoxes <- function(edit.cols, col.spec) {
  
    edit.boxes <- vector(mode = "list", length = nrow(edit.cols))
    
    for (row in 1:nrow(edit.cols)) {
      col <- edit.cols[row,]
      names(edit.boxes)[row] <- col$name
      
      # Create edit box based on column type "select", "text", "notes", "numeric", "time", or "date"
      if (col$type == "text") {
        edit.boxes[col$name] <- list(textInput(col$name, col$label))
      } else if (col$type == "numeric") {
        edit.boxes[col$name] <- list(numericInput(col$name, col$label, value = NA))
      } else if (col$type == "date") {
        edit.boxes[col$name] <- list(dateInput(col$name, col$label, value = NA))
      } else if (col$type == "time") {
        edit.boxes[col$name] <- list(textInput(col$name, col$label))
      } else if (col$type == "notes") {
        edit.boxes[col$name] <- list(textAreaInput(col$name, col$label))
      } else if (col$type == "select") {
        lookup.tbl <- col.spec[[col$name]]$lookup  # get lookup table
        lookup.pk <- col.spec[[col$name]]$lookup.pk  # primary key of lookup table
        lookup.text <- col.spec[[col$name]]$lookup.text  # column in lookup table to display
  
        options <- setNames(lookup.tbl[[lookup.pk]], lookup.tbl[[lookup.text]])  # dropdown options with primary key as value
  
        edit.boxes[col$name] <- list(selectInput(col$name, col$label,
                                choices = c("", options),
                                selected = NA))
      }
    }
    return(edit.boxes)
  }

  # Add edit boxes to UI
  output$data.edit <- renderUI({
    data.in()
    edit.boxes <- makeEditBoxes(edit.cols, col.spec)
    tagList(edit.boxes)
  })
  
}
