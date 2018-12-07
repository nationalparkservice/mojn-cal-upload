library(tidyverse)
library(pool)

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


SpCond.col.spec <- list(CalibrationDate = list(label = "Date",
                                               view = TRUE,
                                               edit = TRUE,
                                               type = "date"),
                        CalibrationTime = list(label = "Time",
                                               view = TRUE,
                                               edit = TRUE,
                                               type = "text"),
                        StandardValue_microS_per_cm = list(label = "Standard (\u03bcS/cm)",
                                                           view = TRUE,
                                                           edit = TRUE,
                                                       type = "numeric"),
                        PreCalibrationReading_microS_per_cm = list(label = 'Pre-cal (\u03bcS/cm)',
                                                           view = TRUE,
                                                           edit = TRUE,
                                                           type = "numeric"),
                        PostCalibrationReading_microS_per_cm = list(label = "Post-cal (\u03bcS/cm)",
                                                                   view = TRUE,
                                                                   edit = TRUE,
                                                                   type = "numeric"),
                        SpCondInstrumentID = list(label = "Instrument",
                                                  view = TRUE,
                                                  edit = TRUE,
                                                  type = "select",
                                                  lookup = db.ref.wqinstr %>% collect(),
                                                  lookup.pk = "ID",
                                                  lookup.text = "Label"),
                        Notes = list(label = "Notes",
                                     view = TRUE,
                                     edit = TRUE,
                                     type = "notes"),
                        DateCreated = list(label = "Date Created",
                                           view = TRUE,
                                           edit = FALSE,
                                           type = "date")
                        )

# Calibration table spec
calib.table.spec <- list(CalibrationSpCond = list(table.name = "CalibrationSpCond",
                                                  search.string = "*_CalibrationSpCond.csv",
                                                  col.types = cols(CalibrationDate = col_character(),
                                                                   CalibrationTime = col_time(),
                                                                   StandardValue_microS_per_cm = col_double(),
                                                                   PreCalibrationReading_microS_per_cm = col_double(),
                                                                   PostCalibrationReading_microS_per_cm = col_double(),
                                                                   SpCondInstrumentGUID = col_character(),
                                                                   Notes = col_character(),
                                                                   GUID = col_character(),
                                                                   DateCreated = col_character()),
                                                  data.manip = function(data)({
                                                    data <- data %>%
                                                      mutate(CalibrationTime = format(CalibrationTime, "%H:%M:%S"),
                                                             CalibrationDate = as.Date(CalibrationDate, format = "%m/%d/%Y")) %>%  # Format dates and times so they display properly
                                                      left_join(db.ref.wqinstr, by = c("SpCondInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
                                                      select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -SpCondInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
                                                      rename(SpCondInstrumentID = ID)
                                                    return(data)
                                                  })
                                                  ),
                         CalibrationDO = list(table.name = "CalibrationDO",
                                              search.string = "*_CalibrationDO.csv",
                                              col.types = cols(CalibrationDate = col_character(),
                                                               CalibrationTime = col_time(),
                                                               PreCalibrationTemperature_C = col_double(),
                                                               PreCalibrationReading_percent = col_double(),
                                                               PostCalibrationTemperature_C = col_double(),
                                                               PostCalibrationReading_percent = col_double(),
                                                               DOInstrumentGUID = col_character(),
                                                               Notes = col_character(),
                                                               GUID = col_character(),
                                                               DateCreated = col_character()),
                                              data.manip = function(data)({
                                                data <- data %>%
                                                  mutate(CalibrationTime = format(CalibrationTime, "%H:%M:%S"),
                                                         CalibrationDate = as.Date(CalibrationDate, format = "%m/%d/%Y")) %>%  # Format dates and times so they display properly
                                                  left_join(db.ref.wqinstr, by = c("DOInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
                                                  select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -DOInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
                                                  rename(DOInstrumentID = ID)
                                                return(data)
                                              })
                                              ),
                         CalibrationpH = list(table.name = "CalibrationpH",
                                              search.string = "*_CalibrationpH.csv",
                                              col.types = cols(CalibrationDate = col_character(),
                                                               CalibrationTime = col_time(),
                                                               StandardValue_pH = col_integer(),
                                                               TemperatureCorrectedStd_pH = col_double(),
                                                               PreCalibrationTemperature_C = col_double(),
                                                               PreCalibrationReading_pH = col_double(),
                                                               PostCalibrationTemperature_C = col_double(),
                                                               PostCalibrationReading_pH = col_double(),
                                                               DOInstrumentGUID = col_character(),
                                                               Notes = col_character(),
                                                               GUID = col_character(),
                                                               DateCreated = col_character()),
                                              data.manip = function(data)({
                                                data <- data %>%
                                                  mutate(CalibrationTime = format(CalibrationTime, "%H:%M:%S"),
                                                         CalibrationDate = as.Date(CalibrationDate, format = "%m/%d/%Y")) %>%  # Format dates and times so they display properly
                                                  left_join(db.ref.wqinstr, by = c("pHInstrumentGUID" = "GUID"), copy = TRUE) %>%  # Join to WQ instrument table
                                                  select(-Summary, -Model, -Manufacturer, -NPSPropertyTag, -IsActive, -pHInstrumentGUID, -Label) %>%  # Get rid of unnecessary columns
                                                  rename(pHInstrumentID = ID)
                                                return(data)
                                              }))
                         )