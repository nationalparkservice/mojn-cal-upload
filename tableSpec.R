library(tidyverse)

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