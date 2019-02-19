library(shiny)
library(odbc)
library(dbplyr)
library(tidyverse)
library(pool)
library(DT)

#---------------------------------------
# Functions
#---------------------------------------

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

makeEditBoxes <- function(session, edit.cols, col.spec) {
  # Function that takes a list of columns and the column specification for a table and returns a list of UI input boxes based on column types
  # and labeled with column names.
  #
  # Args:
  #   session: Shiny session.
  #   edit.cols: A data frame of column names, labels, and types, indicating which columns in col.spec should be used to generate input boxes.
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
  #   A tag list of UI input boxes.
  
  # Initialize edit box list.
  edit.boxes <- vector(mode = "list", length = nrow(edit.cols))
  
  for (row in 1:nrow(edit.cols)) {
    col <- edit.cols[row,]
    names(edit.boxes)[row] <- col$name
    
    # Create edit box based on column type "select", "text", "notes", "numeric", "time", or "date"
    if (col$type == "text") {
      edit.boxes[col$name] <- list(textInput(inputId = session$ns(col$name), label = col$label, value = ""))
    } else if (col$type == "numeric") {
      edit.boxes[col$name] <- list(numericInput(inputId = session$ns(col$name), label = col$label, value = NA))
    } else if (col$type == "date") {
      edit.boxes[col$name] <- list(dateInput(inputId = session$ns(col$name), label = col$label, value = NA))
    } else if (col$type == "time") {
      edit.boxes[col$name] <- list(textInput(inputId = session$ns(col$name), label = col$label, value = ""))
    } else if (col$type == "notes") {
      edit.boxes[col$name] <- list(textAreaInput(inputId = session$ns(col$name), label = col$label, value = ""))
    } else if (col$type == "select") {
      lookup.tbl <- col.spec[[col$name]]$lookup  # get lookup table
      lookup.pk <- col.spec[[col$name]]$lookup.pk  # primary key of lookup table
      lookup.text <- col.spec[[col$name]]$lookup.text  # column in lookup table to display
      
      options <- setNames(lookup.tbl[[lookup.pk]], lookup.tbl[[lookup.text]])  # dropdown options with primary key as value
      
      edit.boxes[col$name] <- list(selectInput(inputId = session$ns(col$name), label = col$label,
                                               choices = c("", options),
                                               selected = NA))
    }
  }
  
  # Convert edit boxes to tag list and add cancel/save/delete buttons
  edit.boxes <- tagList(edit.boxes)
  delete.button <- actionButton(session$ns("delete"), "Delete")
  cancel.button <- actionButton(session$ns("cancel"), "Cancel")
  save.button <- actionButton(session$ns("save"), "Save")
  edit.boxes <- tagAppendChildren(edit.boxes, delete.button, cancel.button, save.button)
  
  return(edit.boxes)
}

updateEditBoxes <- function(session, edit.cols, row.selected, data) {
  # Updates edit boxes with data from a specified row in a reactive data frame. Clears boxes if no row specified.
  #
  # Args:
  #   session: Shiny session.
  #   edit.cols: A data frame of column names, labels, and types, indicating which columns of data correspond to edit boxes.
  #   row.selected: An integer indicating the row of data with which to populate the edit boxes.
  #   data: A reactive data frame.
  # Returns:
  #   A tag list of UI input boxes.
  
  for (row in 1:nrow(edit.cols)) {
    
    col <- edit.cols[row,]
    
    # If a row is selected, set the update value to that row of the current column. Otherwise, set it to "" or NA depending on the data type.
    if (length(row.selected == 1)) {
      update.value <- data[[row.selected, col$name]]
    } else if (col$type %in% c("text", "time", "notes", "select")) {
      update.value <- ""
    } else {
      update.value <- NA
    }
    
    # Create edit box based on column type "select", "text", "notes", "numeric", "time", or "date"
    if (col$type == "text") {
      updateTextInput(session = session, inputId = col$name, value = update.value)
    } else if (col$type == "numeric") {
      updateNumericInput(session = session, inputId = col$name, value = update.value)
    } else if (col$type == "date") {
      updateDateInput(session = session, inputId = col$name, value = update.value)
    } else if (col$type == "time") {
      updateTextInput(session = session, inputId = col$name, value = update.value)
    } else if (col$type == "notes") {
      updateTextAreaInput(session = session, inputId = col$name, value = update.value)
    } else if (col$type == "select") {
      updateSelectInput(session = session, inputId = col$name, selected = update.value)
    }
  }
}


#---------------------------------------
# Modules
#---------------------------------------

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
    uiOutput(ns("data.edit"))  # Dynamically generated edit boxes will go here
  )
  
}

# Data view and edit module server function
dataViewAndEdit <- function(input, output, session, data, col.spec) {
  # Module for viewing data in a dataframe and selecting/editing rows
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
  
  data.in <- reactiveVal({
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
    data.in()
    
    isolate({
      # only show data table if there are data to display
      if (nrow(data.in()) > 0) {
  
          for (col in fk.cols) {
            lookup.tbl <- col.spec[[col]]$lookup  # get lookup table
            lookup.pk <- col.spec[[col]]$lookup.pk  # primary key of lookup table
            lookup.text <- col.spec[[col]]$lookup.text  # column in lookup table to display
            
            # Join data table to lookup table
            data.view <- data.in() %>%
              left_join(lookup.tbl, by = setNames(lookup.pk, col))
            
            # Rename display columns from lookups to something meaningful
            data.view <- data.view %>%
              setnames(old = lookup.text, new = paste0(col, "_lookup"))
          }
          
          # Create the data table
          data.view %>%
            select(table.cols$name) %>%  # Only use columns where view = TRUE in the column spec
            singleSelectDT(col.names = table.cols$label) # Use friendly labels from column spec as table headers
      }
    })
    
  })
  
  # Add edit boxes to UI
  output$data.edit <- renderUI({
    # only show edit boxes if data are present
    if (nrow(data.in()) > 0) {
      makeEditBoxes(session, edit.cols, col.spec)
    }
  })
  
  # Create a reactive expression so that observeEvent fires when a row is deselected
  rows_selected <- reactive(!is.null(input$data.view_rows_selected))
  
  # Populate editable input boxes with values from the selected row
  observeEvent(rows_selected(), {
    # TODO: Check if there are unsaved changes in the input boxes before deselecting a row or selecting a new row
    updateEditBoxes(session, edit.cols, input$data.view_rows_selected, data.in())
  })
  
  # Data table proxy for selecting rows
  dt.proxy <- dataTableProxy("data.view")
  
  # Save changes to data
  # TODO: Figure out why row gets deselected when data are saved
  observeEvent(input$save, {
    # Save the row number that was selected
    selected.row <- input$data.view_rows_selected
    
    # Don't do anything if there isn't exactly one selected row
    if (length(selected.row) == 1) {
      # Get the new values from the input boxes and coerce them to the correct data types
      updated.row <- data_frame()
      for (input.name in edit.cols$name) {
        input.type <- edit.cols$type[edit.cols$name == input.name]
        value <- input[[input.name]]
        
        if (input.type == "numeric" | 
            (input.type == "select" & !is.na(as.numeric(value)))) {
          # If input is numeric or select with numeric pk, convert to numeric type
          value <- as.numeric(value)
        } else if (input.type == "date") {
          value <- format(value)
        } else if (input.type == "time") {
          value <- as.POSIXlt.character(value, tryFormats = c("%I:%M %p","%I:%M:%S %p", "%I %p", "%H:%M", "%H:%M:%S")) %>%
            format("%H:%M:%S")
        }
        
        updated.row[1, input.name] <- value
      }
      
      #Assign the new values to the data frame
      new.data <- data.in()
      data.changed <- any(new.data[input$data.view_rows_selected, edit.cols$name] != updated.row[1, ])
      if (data.changed || is.na(data.changed)) {
        new.data[input$data.view_rows_selected, edit.cols$name] <- updated.row[1, ]
        data.in(new.data)
        showNotification("Data saved", type = "message")
      } else {
        showNotification("No changes to save", type = "warning") 
      }
      
      # Re-select the row that was selected
      dt.proxy %>% selectRows(selected.row)
    }
  })
  
  # Cancel changes to data
  observeEvent(input$cancel, {
    dt.proxy %>% selectRows(NULL)
  })
  
  # Delete a row of data
  observeEvent(input$delete, {
    # If no rows are selected, don't do anything
    if (!is.null(input$data.view_rows_selected)) {
      # Save the row number that was selected
      selected.row <- input$data.view_rows_selected
      # Prompt user to confirm deletion
      showModal({
        modalDialog(
          h3("Confirm deletion"),
          p("Are you sure that you want to delete the selected row of data?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("conf.delete"), "Delete")
          ),
          easyClose = FALSE,
          size = "m"
        )
      })
      # Re-select the row that was selected for deletion (the modal dialog will otherwise clear row selections)
      dt.proxy %>% selectRows(selected.row)
    }
    
  })
  
  observeEvent(input$conf.delete, {
    # Delete the selected row
    new.data <- data.in()
    new.data <- new.data[-input$data.view_rows_selected, ]
    data.in(new.data)
    
    removeModal()
  })
  
  return(data.in)
}

# Data upload module UI
dataUploadUI <- function(id) {
  # UI module for viewing final data and uploading it to a database.
  #
  # Args:
  #   id: The namespace for the module
  
  ns <- NS(id)
  
  tagList(
    h3("Final data"),
    dataTableOutput(ns("final.data")),  # Data table UI for viewing data and selecting a row
    actionButton(ns("submit"), "Submit data")
  )
  
}

# Data upload module server function
dataUpload <- function(input, output, session, data, table.spec) {
  # Module for viewing final data and uploading it to a database.
  #
  # Args:
  #   input, output, session: required parameters for Shiny server function.
  #   data: The data table to be uploaded.
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
  #   A boolean value indicating whether data upload was successful
  
  final.data <- reactive({
    # Do nothing if no data present
    validate(need(data(), message = FALSE))
    data()
  })
  
    # Populate table
  output$final.data <- renderDT({
    final.data()
    
    # only show data table if there are data to display
    if (nrow(final.data()) > 0) {
      final.data() %>%
        select(names(table.spec$col.spec)) %>%
        singleSelectDT(col.names = names(table.spec$col.spec))
    }
    
  })
  
  # Delete a row of data
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
  
  observeEvent(input$conf.upload, {
    # Attempt to append data to table in database
    tryCatch({
      # If successful, display success message
      # TODO: disable repeat uploads
      table.spec$data.upload(final.data())
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
      
    }
    )
  })
  
}