#####################################
## Data for Ball Mapper Analysis
#####################################


# General Selection of Analysis Data --------------------------------------
# From our Sources section, we select:
dataSources <- c("PPI_All",
                 "PID",
                 "GDP",
                 "CPI",
                 "unemployment",
                 "FFR",
                 "housingStarts")


analysisData <- all_annual[dataSources]

# Mutate cols in `change_tbl` to calculate pct annual change
change_tbl <- c("PPI_All", "CPI", "housingStarts")
analysisData <- map2(analysisData, names(analysisData),
                     ~ {
                         if(.y %in% change_tbl) {
                             seriesID <- unique(.x$series_id)

                             .x |>
                                 mutate(pctChange =
                                            (avgA - lag(avgA))/lag(avgA)) |>
                                 rename_with(~ paste0(., "_", seriesID),
                                             c(avgA, pctChange)
                                 )
                             } else {
                                 .x
                             }
                         }
)


# Changing Specific Table Cols --------------------------------------------
analysisData[["PPI_All"]] <- analysisData[["PPI_All"]] |>
    select(year, pctChange_PPIACO)

# Reduce Personal Income and it's Disposition cols. Example how to read:
# Large category:
#   - included in category but not keeping
#   - another col included not keeping
#   - etc.

# Supplements to Wages and Salaries
# - Employer contrib. pension and insurance
# - employer contrib. for gov social insurance

# Personal Income Receipts on assets
# - Personal Interest Income
# - Personal Dividend Income

# Personal Current transfer receipts
# - Govnment social benefits:
#       - Social security
#       - Medicare/Medicaid
#       - Unemployment insurance/Veteran's benefits/other
# - Other current transfers (e.g. in-kind donations)
# - Less: contrib. for gov. social insurance

analysisData[["PID"]] <- analysisData[["PID"]] |>
    select(year, c(45:50, 54:57, 60, 70:72, 78)) |>
    rename_with(.cols = starts_with("Less:_"),
                .fn = ~str_remove(.x, "Less:_")) |>
    rename_with(.cols = starts_with("Equals:_"),
                .fn = ~str_remove(.x, "Equals:_"))

# Reducing GDP cols for computational and analysis purposes

# GDP, current dollars
analysisData[["GDP"]] <- analysisData[["GDP"]] |>
    select(-26)

analysisData[["CPI"]] <- analysisData[["CPI"]] |>
    select(year, pctChange_CPIAUCSL)

analysisData[["unemployment"]] <- analysisData[["unemployment"]] |>
    rename("unrate" = avgA) |>
    select(year, unrate)

analysisData[["FFR"]] <- analysisData[["FFR"]] |>
    select(year, rate_FFR)

analysisData[["housingStarts"]] <- analysisData[["housingStarts"]] |>
    select(year, pctChange_HOUST)

# Reduce tables to select years
analysisYears <- c(1960:2024)
analysisData <- map(analysisData, ~ filter(.x, year %in% analysisYears))

# Reduce and combine tables
finalData <- reduce(analysisData, full_join, by = "year") |>
    select(-starts_with(c("Less:", "")))

