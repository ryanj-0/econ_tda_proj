##########################################################
## Loading Data From Federal Reserve Economic Data (FRED):
## - Federal Funds Effective Rate: Monthly, Yearly
## - Consumer Price Index (CPI)
########################################################


# Federal Funds Effective Rate --------------------------------------------
# 1954 - Current

# Monthly Rate
FFR_M <-
    fredr(series_id = "FEDFUNDS")


# Quarterly Rate
# Transformed using arithmetic mean with quarters defined:
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
    mutate(avgRate = mean(value)) |>
    ungroup() |>
    select(series_id, year, quarter, avgRate) |>
    unique()

# Yearly Rate
FFR_A <-
    fredr(series_id = "RIFSPFFNA") |>
    mutate(year = str_extract(date, "[0-9]{4}")) |>
    rename(rate = value) |>
    select(series_id, year, rate)


# Consumer Price Index ----------------------------------------------------

# Monthly Consumer Price Index (CPI)
CPI_M <-
    fredr(series_id = "CPIAUCSL")

# Quarterly
# Transformed using arithmetic mean with quarters defined:
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
    mutate(avgRate = mean(value)) |>
    ungroup() |>
    select(series_id, year, quarter, avgRate) |>
    unique()

CPI_A <-
    CPI_M |>
    mutate(year = format(date, "%Y") |> as.numeric()) |>
    group_by(year) |>
    mutate(avgRate = mean(value)) |>
    ungroup() |>
    select(series_id, year, avgRate) |>
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

# Annual
# Transformed using arithmetic mean
ECI_A <-
    ECI_Q |>
    group_by(year) |>
    mutate(avg = mean(value)) |>
    ungroup() |>
    select(series_id, year, avg) |>
    unique()

