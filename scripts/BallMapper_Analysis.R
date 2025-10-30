#####################################
## Data for Ball Mapper Analysis
#####################################

# From our Sources section, we select:
dataSources <- c("PPI_All",
                 "PID",
                 "GDP",
                 "CPI",
                 "unemployment",
                 "FFR",
                 "housingStarts")

analysisData <- map(dataSources, ~all_annual[[.x]])
names(analysisData) <- dataSources

# Reduce tables to select years
analysisYears <- c(1959:2024)

analysisData <- map(analysisData, ~ filter(.x, year %in% analysisYears))


# Col. to change to pct change
change_cols <- c("PPI_All",
                 "PID",
                 "CPI",
                 "housingStarts")

changtToPercent <- map(analysisData,
                       ~ {
                           if(.x %in% change_cols) {
                               mutate(.x, chnage)
                           }
                       })
