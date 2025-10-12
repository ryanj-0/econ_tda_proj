##########################################################
## Loading Data From Federal Reserve Economic Data (FRED):
## - Federal Funds Effective Rate: Monthly, Yearly
## - Consumer Price Index (CPI)
## - Employment Cost Index (ECI)


## Notes on calculations:
##     Some data is only given on a monthly frequency.
## We are interested in Quarterly and Annual data. So to
## obtain a value for our interested time frequency, we
## take the arithmetic mean. Comments below will indicate
## when series is calculated.
##########################################################


# Federal Funds Effective Rate --------------------------------------------

# Monthly Rate
FFR_M <-
    fredr(series_id = "FEDFUNDS")


# Quarterly Rate (calculated)
# Q1 = 1:3, Q2 = 4:6, Q3 = 7:9, Q4 = 10:12
FFR_Q <-
    FFR_M |>
    mutate(year = format(date, "%Y") |> as.numeric(),
           month = format(date, "%m") |> as.numeric(),
           quarter = case_when(month %in% c(1:3) ~ 1,
                               month %in% c(4:6) ~ 2,
                               month %in% c(7:9) ~ 3,
                               month %in% c(10:12) ~ 4),
           yearQuarter = paste0(year, quarter) #need a unique groupby var.
    ) |>
    group_by(yearQuarter) |>
    mutate(avgRate_Q = mean(value)) |>
    ungroup() |>
    select(series_id, year, quarter, avgRate_Q) |>
    unique()

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
CPI_Q <-
    CPI_M |>
    mutate(year = format(date, "%Y") |> as.numeric(),
           month = format(date, "%m") |> as.numeric(),
           quarter = case_when(month %in% c(1:3) ~ 1,
                               month %in% c(4:6) ~ 2,
                               month %in% c(7:9) ~ 3,
                               month %in% c(10:12) ~ 4),
           yearQuarter = paste0(year, quarter) # need a unique group_by var
    ) |>
    group_by(yearQuarter) |>
    mutate(avgQ = mean(value)) |>
    ungroup() |>
    select(series_id, year, quarter, avgQ) |>
    unique()

# Annual (calculated)
CPI_A <-
    CPI_Q
    group_by(year) |>
    mutate(avgA = mean(avgQ)) |>
    ungroup() |>
    select(series_id, year, avgA) |>
    unique()


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
housingStart_M <-
    fredr(series_id = "HOUST")

# Quarterly (calculated)
housingStarts_Q <-
    housingStart_M |>
    mutate(year = format(date, "%Y") |> as.numeric(),
           month = format(date, "%m") |> as.numeric(),
           quarter = case_when(month %in% c(1:3) ~ 1,
                               month %in% c(4:6) ~ 2,
                               month %in% c(7:9) ~ 3,
                               month %in% c(10:12) ~ 4),
           yearQuarter = paste0(year, quarter) # need a unique group_by var
    ) |>
    group_by(yearQuarter) |>
    mutate(avgQ = mean(value)) |>
    ungroup() |>
    select(series_id, year, quarter, avgQ) |>
    unique()

# Annual (calculated)
housingStarts_A <-
    housingStarts_Q |>
    group_by(year) |>
    mutate(avgA = mean(avgQ)) |>
    ungroup() |>
    select(series_id, year, avgA) |>
    unique()
