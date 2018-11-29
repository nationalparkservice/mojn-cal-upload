library(shiny)
library(odbc)
library(dbplyr)
library(tidyverse)
library(pool)
library(DT)

# CSV file import module UI
# TODO: read from JSON
fileImportInput <- function(id, table.spec) {
  # File import module that reads csv data files into dataframes in a reactiveValues object.
  #
  # Args:
  #   id: The namespace for the module
  #   table.spec: A list of table types (e.g. CalibrationDO, WaterQualityActivity), each a list containing the following:
  #                 search.string: Regular expression specifying which files in file.names correspond to that table type
  #                 col.types: Column specification
  #                 data.manip: Function to manipulate the data. Must take the data table as its first argument.
  #
  # Returns:
  #   A reactiveValues object containing the data read from the input files. Note that duplicate rows will be removed.
  
  
}