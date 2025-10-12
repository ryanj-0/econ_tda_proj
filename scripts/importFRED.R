##########################################################
## Loading Data From Federal Reserve Economic Data (FRED):
## - Federal Funds Effective Rate (FFR)
## - Consumer Price Index (CPI)
## - Employment Cost Index (ECI)
## - Housing Starts
## - Producer Price Index (PPI)
##       - All Commodities
##       - Final Demand


## Notes on calculations:
##     Some data is only given on a monthly frequency.
## We are interested in Quarterly and Annual data. So to
## obtain a value for our interested time frequency, we
## take the arithmetic mean. Comments below will indicate
## when series is calculated.
##     For the Producer Price Index (PPI), we chose to
## include both All Commodities and Final Goods because
## they show us different aspects of the macroeconomy.
##########################################################


# Federal Funds Effective Rate --------------------------------------------

# Monthly Rate
FFR_M <-
    fredr(series_id = "FEDFUNDS")


# Quarterly Rate (calculated)
# Q1 = 1:3, Q2 = 4:6, Q3 = 7:9, Q4 = 10:12
FFR_Q <- changeToQuarterly(FFR_M)

# Yearly Rate
FFR_A <-
    fredr(series_id = "RIFSPFFNA") |>
    mutate(year = str_extract(date, "[0-9]{4}")) |>
    rename(rate = value) |>
    select(series_id, year, rate)


# Consumer Price Index (CPI) ----------------------------------------------


# Monthly
CPI_M <-
    fredr(series_id = "CPIAUCSL")

# Quarterly (calculated)
# Q1 = 1:3, Q2 = 4:6, Q3 = 7:9, Q4 = 10:12
CPI_Q <- changeToQuarterly(CPI_M)

# Annual (calculated)
CPI_A <- changeTOAnnually(CPI_Q)


# Employment Cost Index - Total Compensation ------------------------------

# Quarterly
ECI_Q <-
    fredr(series_id = "ECIALLCIV") |>
    mutate(year = format(date, "%Y") |> as.numeric(),
           month = format(date, "%m") |> as.numeric(),
           quarter = case_when(month %in% c(1:3) ~ 1,
                               month %in% c(4:6) ~ 2,
                               month %in% c(7:9) ~ 3,
                               month %in% c(10:12) ~ 4)) |>
    select(series_id, year, quarter, value)

# Annual (calculated)
ECI_A <-
    ECI_Q |>
    group_by(year) |>
    mutate(avgA = mean(value)) |>
    ungroup() |>
    select(series_id, year, avgA) |>
    unique()


# Housing Starts: Private -------------------------------------------------

# Monthly
housingStarts_M <-
    fredr(series_id = "HOUST")

# Quarterly (calculated)
housingStarts_Q <- changeToQuarterly(housingStarts_M)

# Annual (calculated)
housingStarts_A <- changeTOAnnually(housingStarts_Q)


# Producer Price Index - All Commodities ----------------------------------

# Monthly
PPI_All_M <-
    fredr(series_id = "PPIACO")

# Quarterly (calculated)
PPI_All_Q <- changeToQuarterly(PPI_All_M)

PPI_ALL_A <- changeTOAnnually(PPI_All_Q)


# Producer Price Index - Final Demand -------------------------------------

# Monthly
PPI_Final_M <-
    fredr(series_id = "PPIFIS")

# Quarterly (calculated)
PPI_Final_Q <- changeToQuarterly(PPI_All_M)

PPI_Final_A <- changeTOAnnually(PPI_All_Q)


