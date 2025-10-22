##########################################################
## Loading Data From Federal Reserve Economic Data (FRED):
## - Federal Funds Effective Rate (FFR)
## - Consumer Price Index (CPI)
## - Employment Cost Index (ECI)
## - Housing Starts
## - Producer Price Index (PPI)
##       - All Commodities
##       - Final Demand
## - Unemployment Rate

## Notes on calculations:
##     Some data is only given on a monthly frequency.
## We are interested in Quarterly and Annual data. So to
## obtain a value for our interested time frequency, we
## take the arithmetic mean. Comments below will indicate
## when series is calculated.
##     For the Producer Price Index (PPI), we chose to
## include both All Commodities and Final Goods because
## they show us different aspects of the macroeconomy.

## Notes on Data
##     We try and use seasonally adjusted date when
## available.
##########################################################


# Federal Funds Effective Rate --------------------------------------------

# Monthly Rate
FFR_M <- fredr(series_id = "FEDFUNDS")


# Quarterly Rate (calculated)
# Q1 = 1:3, Q2 = 4:6, Q3 = 7:9, Q4 = 10:12
FFR_Q <- changeToQuarterly(FFR_M)

# Yearly Rate
FFR_A <- fredr(series_id = "RIFSPFFNA") |>
    mutate(year = str_extract(date, "[0-9]{4}")) |>
    rename(rate = value) |>
    select(series_id, year, rate)


# Employment Cost Index - Total Compensation ------------------------------

# Quarterly
ECI_Q <- fredr(series_id = "ECIALLCIV") |>
    mutate(
        year = format(date, "%Y") |> as.numeric(),
        month = format(date, "%m") |> as.numeric(),
        quarter = case_when(
            month %in% c(1:3) ~ 1,
            month %in% c(4:6) ~ 2,
            month %in% c(7:9) ~ 3,
            month %in% c(10:12) ~ 4
        )
    ) |>
    select(series_id, year, quarter, value)

# Annual (calculated)
ECI_A <- ECI_Q |>
    group_by(year) |>
    mutate(avgA = mean(value)) |>
    ungroup() |>
    select(series_id, year, avgA) |>
    unique()


# Provided Monthly Data Only ----------------------------------------------
#     All data in following section is data which is provided monthly and
# is calculated to quarterly and annual based on our calculation notes
# above.

seriesTable <- tibble(
    id = c("CPIAUCSL", "HOUST", "PPIACO", "PPIFIS", "UNRATE"),
    name = c("CPI", "housingStarts", "PPI_All", "PPI_Final", "unemployment")
)

FRED_data <-
    purrr::map(seriesTable$id, ~ {
        monthlyData <- fredr(series_id = .x)
        quarterlyData <- changeToQuarterly(monthlyData)
        annualData <- changeTOAnnually(quarterlyData)
        list(quarterly = quarterlyData, annual = annualData) # return
    }) |>
    set_names(seriesTable$name)



# Recession Dates ---------------------------------------------------------


nberRecessions <-
    fredr(series_id = "USRECQ") |>
    mutate(
        year = format(date, "%Y") |> as.numeric(),
        month = format(date, "%m") |> as.numeric(),
        quarter = case_when(
            month %in% c(1:3) ~ 1,
            month %in% c(4:6) ~ 2,
            month %in% c(7:9) ~ 3,
            month %in% c(10:12) ~ 4
        )
    ) |>
    filter(value == 1) |>
    group_by(year) |>
    mutate(fullYear = n() / 4) |>
    ungroup() |>
    select(series_id, year, quarter, fullYear)
