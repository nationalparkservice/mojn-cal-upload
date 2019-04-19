library(pool)

# Database connection
my.pool <- dbPool(drv = odbc::odbc(),
               Driver = "SQL Server Native Client 11.0",
               Server = "INPLAKE52V\\MOJN",
               Database = "Testing_MOJN_SharedTables",
               Trusted_Connection = "Yes")

onStop(function() {
  poolClose(my.pool)
})

# Load table pointers to calibration data and refs:
db.SpCond <- tbl(my.pool, in_schema("data", "CalibrationSpCond"))
db.DO <- tbl(my.pool, in_schema("data", "CalibrationDO"))
db.pH <- tbl(my.pool, in_schema("data", "CalibrationpH"))
db.ref.wqinstr <- tbl(my.pool, in_schema("ref", "WaterQualityInstrument"))
dropdown.wqinstr <- arrange(db.ref.wqinstr, desc(IsActive), Model) %>% collect()
dropdown.wqinstr <- setNames(dropdown.wqinstr$ID, dropdown.wqinstr$Label)

# Prepare data for insert into the database
prepDataForInsert <- function(data) {
  # Convert data types to make SQL happy
  data$CalibrationDate <- as.POSIXct(data$CalibrationDate, tz = "GMT")
  data$CalibrationTime <- as.POSIXct(paste("1899-12-30 ", data$CalibrationTime), format = format("%Y-%m-%d %H:%M"), tz = "GMT")
  data$Notes[trimws(data$Notes, "both") == ""] <- NA  # Convert blank notes to NA
  # Prevent "NA" being inserted into the database as a string. This seems to only happen when there is a single row in
  # the data frame and a column of type character contains NA. Converting that column to type logical fixes the problem.
  if (all(is.na(data$Notes))) {
    data$Notes <- as.logical(data$Notes)
  }
  
  data
}

# Generate INSERT INTO statement
insertInto <- function(data, db.pool = my.pool) {
  # Generate INSERT INTO statement for each calibration table
  sql.do <- ""
  sql.spcond <- ""
  sql.ph <- ""
  if (nrow(data$CalibrationDO()) > 0) {
    data.do <- prepDataForInsert(data$CalibrationDO())
    
    cols <- paste(names(data.do), collapse = ", ")
    placeholders <- rep("?", length(names(data.do)))
    placeholders <- paste(placeholders, collapse = ", ")
    sql.do <- paste0("INSERT INTO data.CalibrationDO (",
                     cols, ") ",
                     "VALUES (",
                     placeholders,
                     ")")
  }
  if (nrow(data$CalibrationSpCond()) > 0) {
    data.spcond <- prepDataForInsert(data$CalibrationSpCond())
    
    cols <- paste(names(data.spcond), collapse = ", ")
    placeholders <- rep("?", length(names(data.spcond)))
    placeholders <- paste(placeholders, collapse = ", ")
    sql.spcond <- paste0("INSERT INTO data.CalibrationSpCond (", cols, ") ",
                         "VALUES (",
                         placeholders,
                         ")")
  }
  if (nrow(data$CalibrationpH()) > 0) {
    data.ph <- prepDataForInsert(data$CalibrationpH())
    
    cols <- paste(names(data.ph), collapse = ", ")
    placeholders <- rep("?", length(names(data.ph)))
    placeholders <- paste(placeholders, collapse = ", ")
    sql.ph <- paste0("INSERT INTO data.CalibrationpH (",
                     cols, ") ",
                     "VALUES (",
                     placeholders,
                     ")")
  }
  
  poolWithTransaction(pool = db.pool, func = function(conn) {
    # Insert DO
    if (sql.do != "") {
      insert <- dbSendStatement(conn, sql.do)
      dbBind(insert, as.list(data.do))
      dbClearResult(insert)
    }
    
    # Insert SpCond
    if (sql.spcond != "") {
      insert <- dbSendStatement(conn, sql.spcond)
      dbBind(insert, as.list(data.spcond))
      dbClearResult(insert)
    }
    
    # Insert pH
    if (sql.ph != "") {
      insert <- dbSendStatement(conn, sql.ph)
      dbBind(insert, as.list(data.ph))
      dbClearResult(insert)
    }
  })
  
}

# Col specs - it is assumed that all of the columns in the column specification should be uploaded to the database after data review
SpCond.col.spec <- list(CalibrationDate = list(label = "Date",
                                               view = TRUE,
                                               edit = TRUE,
                                               required = TRUE,
                                               type = "date"),
                        CalibrationTime = list(label = "Time",
                                               view = TRUE,
                                               edit = TRUE,
                                               required = TRUE,
                                               type = "time"),
                        StandardValue_microS_per_cm = list(label = "Standard (\u03bcS/cm)",
                                                           view = FALSE,
                                                           edit = TRUE,
                                                           required = TRUE,
                                                           type = "numeric"),
                        PreCalibrationReading_microS_per_cm = list(label = 'Pre-cal (\u03bcS/cm)',
                                                                   view = FALSE,
                                                                   edit = TRUE,
                                                                   required = TRUE,
                                                                   type = "numeric"),
                        PostCalibrationReading_microS_per_cm = list(label = "Post-cal (\u03bcS/cm)",
                                                                    view = FALSE,
                                                                    edit = TRUE,
                                                                    required = TRUE,
                                                                    type = "numeric"),
                        SpCondInstrumentID = list(label = "Instrument",
                                                  view = TRUE,
                                                  edit = TRUE,
                                                  required = TRUE,
                                                  type = "select",
                                                  lookup = db.ref.wqinstr %>% collect(),
                                                  lookup.pk = "ID",
                                                  lookup.text = "Label"),
                        Notes = list(label = "Notes",
                                     view = FALSE,
                                     edit = TRUE,
                                     required = FALSE,
                                     type = "notes"),
                        DateCreated = list(label = "Date Created",
                                           view = FALSE,
                                           edit = FALSE,
                                           type = "date")
)

DO.col.spec <- list(CalibrationDate = list(label = "Date",
                                           view = TRUE,
                                           edit = TRUE,
                                           required = TRUE,
                                           type = "date"),
                    CalibrationTime = list(label = "Time",
                                           view = TRUE,
                                           edit = TRUE,
                                           required = TRUE,
                                           type = "time"),
                    BarometricPressure_mmHg = list(label = 'Barometric press. (mmHg)',
                                                   view = FALSE,
                                                   edit = TRUE,
                                                   required = FALSE,
                                                   type = "numeric"),
                    PreCalibrationTemperature_C = list(label = 'Pre-cal temp (C)',
                                                       view = FALSE,
                                                       edit = TRUE,
                                                       required = FALSE,
                                                       type = "numeric"),
                    PreCalibrationReading_percent = list(label = 'Pre-cal (%)',
                                                         view = FALSE,
                                                         edit = TRUE,
                                                         required = TRUE,
                                                         type = "numeric"),
                    PostCalibrationTemperature_C = list(label = 'Post-cal temp (C)',
                                                        view = FALSE,
                                                        edit = TRUE,
                                                        required = FALSE,
                                                        type = "numeric"),
                    PostCalibrationReading_percent = list(label = "Post-cal (%)",
                                                          view = FALSE,
                                                          edit = TRUE,
                                                          required = TRUE,
                                                          type = "numeric"),
                    DOInstrumentID = list(label = "Instrument",
                                          view = TRUE,
                                          edit = TRUE,
                                          required = TRUE,
                                          type = "select",
                                          lookup = db.ref.wqinstr %>% collect(),
                                          lookup.pk = "ID",
                                          lookup.text = "Label"),
                    Notes = list(label = "Notes",
                                 view = FALSE,
                                 edit = TRUE,
                                 required = FALSE,
                                 type = "notes"),
                    DateCreated = list(label = "Date Created",
                                       view = FALSE,
                                       edit = FALSE,
                                       required = FALSE,
                                       type = "date")
)

pH.col.spec <- list(CalibrationDate = list(label = "Date",
                                           view = TRUE,
                                           edit = TRUE,
                                           required = TRUE,
                                           type = "date"),
                    CalibrationTime = list(label = "Time",
                                           view = TRUE,
                                           edit = TRUE,
                                           required = TRUE,
                                           type = "time"),
                    StandardValue_pH = list(label = "Std.",
                                            view = TRUE,
                                            edit = TRUE,
                                            required = TRUE,
                                            type = "numeric"),
                    TemperatureCorrectedStd_pH = list(label = "Temp. corrected std.",
                                                      view = FALSE,
                                                      edit = TRUE,
                                                      required = FALSE,
                                                      type = "numeric"),
                    PreCalibrationTemperature_C = list(label = 'Pre-cal temp (C)',
                                                       view = FALSE,
                                                       edit = TRUE,
                                                       required = FALSE,
                                                       type = "numeric"),
                    PreCalibrationReading_pH = list(label = 'Pre-cal pH',
                                                    view = FALSE,
                                                    edit = TRUE,
                                                    required = TRUE,
                                                    type = "numeric"),
                    PostCalibrationTemperature_C = list(label = 'Post-cal temp (C)',
                                                        view = FALSE,
                                                        edit = TRUE,
                                                        required = FALSE,
                                                        type = "numeric"),
                    PostCalibrationReading_pH = list(label = "Post-cal pH",
                                                     view = FALSE,
                                                     edit = TRUE,
                                                     required = TRUE,
                                                     type = "numeric"),
                    pHInstrumentID = list(label = "Instrument",
                                          view = TRUE,
                                          edit = TRUE,
                                          required = TRUE,
                                          type = "select",
                                          lookup = db.ref.wqinstr %>% collect(),
                                          lookup.pk = "ID",
                                          lookup.text = "Label"),
                    Notes = list(label = "Notes",
                                 view = FALSE,
                                 edit = TRUE,
                                 required = FALSE,
                                 type = "notes"),
                    DateCreated = list(label = "Date Created",
                                       view = FALSE,
                                       edit = FALSE,
                                       required = FALSE,
                                       type = "date")
)

# Calibration table spec
table.spec <- list(CalibrationDO = list(table.name = "CalibrationDO",
                                        display.name = "Dissolved Oxygen",
                                        search.string = "*_CalibrationDO.csv",
                                        col.types = cols(CalibrationDate = col_datetime(format = "%m/%d/%Y"),
                                                         CalibrationTime = col_time(format = "%H:%M:%S"),
                                                         PreCalibrationTemperature_C = col_double(),
                                                         PreCalibrationReading_percent = col_double(),
                                                         PostCalibrationTemperature_C = col_double(),
                                                         PostCalibrationReading_percent = col_double(),
                                                         BarometricPressure_mmHg = col_double(),
                                                         DOInstrumentGUID = col_character(),
                                                         Notes = col_character(),
                                                         GUID = col_character(),
                                                         DateCreated = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")),
                                        col.spec = DO.col.spec,
                                        data.manip = function(data)({
                                          data <- data %>%
                                            mutate(CalibrationTime = format(as.POSIXct(CalibrationTime, format = "%H:%M"), "%H:%M"),
                                                   CalibrationDate = format(CalibrationDate, "%Y-%m-%d"),
                                                   DateCreated = format(DateCreated, "%Y-%m-%d %H:%M:%S")) %>%  # Format dates and times so they display properly
                                            left_join(db.ref.wqinstr, by = c("DOInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
                                            select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -DOInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
                                            rename(DOInstrumentID = ID)
                                          return(data)
                                        })),
                   CalibrationpH = list(table.name = "CalibrationpH",
                                        display.name = "pH",
                                        search.string = "*_CalibrationpH.csv",
                                        col.types = cols(CalibrationDate = col_datetime(format = "%m/%d/%Y"),
                                                         CalibrationTime = col_time(format = "%H:%M:%S"),
                                                         StandardValue_pH = col_integer(),
                                                         TemperatureCorrectedStd_pH = col_double(),
                                                         PreCalibrationTemperature_C = col_double(),
                                                         PreCalibrationReading_pH = col_double(),
                                                         PostCalibrationTemperature_C = col_double(),
                                                         PostCalibrationReading_pH = col_double(),
                                                         pHInstrumentGUID = col_character(),
                                                         Notes = col_character(),
                                                         GUID = col_character(),
                                                         DateCreated = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")),
                                        col.spec = pH.col.spec,
                                        data.manip = function(data)({
                                          data <- data %>%
                                            mutate(CalibrationTime = format(as.POSIXct(CalibrationTime, format = "%H:%M"), "%H:%M"),
                                                   CalibrationDate = format(CalibrationDate, "%Y-%m-%d"),
                                                   DateCreated = format(DateCreated, "%Y-%m-%d %H:%M:%S")) %>%  # Format dates and times so they display properly
                                            left_join(db.ref.wqinstr, by = c("pHInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
                                            select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -pHInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
                                            rename(pHInstrumentID = ID)
                                          return(data)
                                        })),
                   CalibrationSpCond = list(table.name = "CalibrationSpCond",
                                            display.name = "Specific Conductance",
                                            search.string = "*_CalibrationSpCond.csv",
                                            col.types = cols(CalibrationDate = col_datetime(format = "%m/%d/%Y"),
                                                             CalibrationTime = col_time(format = "%H:%M:%S"),
                                                             StandardValue_microS_per_cm = col_double(),
                                                             PreCalibrationReading_microS_per_cm = col_double(),
                                                             PostCalibrationReading_microS_per_cm = col_double(),
                                                             SpCondInstrumentGUID = col_character(),
                                                             Notes = col_character(),
                                                             GUID = col_character(),
                                                             DateCreated = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")),
                                            col.spec = SpCond.col.spec,
                                            data.manip = function(data)({
                                              data <- data %>%
                                                mutate(CalibrationTime = format(as.POSIXct(CalibrationTime, format = "%H:%M"), "%H:%M"),
                                                       CalibrationDate = format(CalibrationDate, "%Y-%m-%d"),
                                                       DateCreated = format(DateCreated, "%Y-%m-%d %H:%M:%S")) %>%  # Format dates and times so they display properly
                                                left_join(db.ref.wqinstr, by = c("SpCondInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
                                                select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -SpCondInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
                                                rename(SpCondInstrumentID = ID)
                                              return(data)
                                            }))
)