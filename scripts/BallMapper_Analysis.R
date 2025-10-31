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

analysisNames <- names(analysisData)
change_tbl <- c("PPI_ALL", "CPI", "housingStarts")
# Reduce tables to select years
analysisYears <- c(1959:2024)

analysisData <- map(dataSources, ~all_annual[[.x]]) |>
    map2(analysisData, names(analysisData),
         ~ {
             if(.y %in% change_tbl) {
                 mutate(.x,
                        pctChange = (avgA - lag(avgA))/lag(avgA))
             } else {
                 .x
             }
         }
    )



analysisData <- map(analysisData, ~ filter(.x, year %in% analysisYears))


