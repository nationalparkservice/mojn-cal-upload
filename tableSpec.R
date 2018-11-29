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
                         CalibrationDO = list()
                         )