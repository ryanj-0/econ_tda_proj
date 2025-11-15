########################################################
## Loading Data From Beureau of Economic Analysis (BEA):
## National Income and Product Accounts (NIPA) Tables
## - GDP
## - Personal Income and Its Disposition
########################################################

# Global config for NIPA Tables
nipaConfig <- list(
    'UserID' = my_beaKey,
    'Method' = 'GetData',
    # Get All Years Available
    'Year' = 'ALL',
    'ResultFormat' = 'json'
)


# GDP ---------------------------------------------------------------------

# Annual GDP
gdpA_list <- append(nipaConfig,
                    list(
                        'datasetname' = 'NIPA',
                        'TableName' = 'T10101',
                        'Frequency' = 'A'
                    ))

GDP_A <- beaGet(gdpA_list) |>
    pivot_longer(
        cols = !(TableName:UNIT_MULT),
        names_to = "time",
        values_to = "pctChange"
    ) |>
    mutate(
        year = str_extract(time, "[0-9]{4}") |> as.numeric(),
        LineDescription = case_when(
            LineNumber == 17 ~ "ExportGoods",
            LineNumber == 18 ~ "ExportServices",
            LineNumber == 20 ~ "ImportGoods",
            LineNumber == 21 ~ "ImportServices",
            .default = LineDescription
        )
    ) |>
    select(LineDescription, year, pctChange) |>
    pivot_wider(names_from = LineDescription, values_from = pctChange) |>
    rename_with(~ gsub(" ", "_", .x)) |>
    rename_with(~ paste0(.x, "_GDP"), -year)

# GDP Quartly
gdpQ_list <- append(nipaConfig,
                    list(
                        'datasetname' = 'NIPA',
                        'TableName' = 'T10101',
                        'Frequency' = 'Q'
                    ))

GDP_Q <- beaGet(gdpQ_list) |>
    pivot_longer(
        cols = !(TableName:UNIT_MULT),
        names_to = "time",
        values_to = "pctChange"
    ) |>
    mutate(
        year = str_extract(time, "[0-9]{4}", ) |> as.numeric(),
        quarter = str_extract(time, "\\d$") |> as.numeric(),
        LineDescription = case_when(
            LineNumber == 17 ~ "ExportGoods",
            LineNumber == 18 ~ "ExportServices",
            LineNumber == 20 ~ "ImportGoods",
            LineNumber == 21 ~ "ImportServices",
            .default = LineDescription
        )
    ) |>
    select(LineDescription, year, quarter, pctChange) |>
    pivot_wider(names_from = LineDescription, values_from = pctChange) |>
    rename_with(~ gsub(" ", "_", .x)) |>
    rename_with(~ paste0(.x, "_GDP"), -c(year, quarter))


# Personal Income ---------------------------------------------------------

# Persional Income and Outlays Annual
pidA_list <- append(nipaConfig,
                    list(
                        'datasetname' = 'NIPA',
                        'TableName' = 'T20100',
                        'Frequency' = 'A'
                    ))


PID_A <- beaGet(pidA_list) |>
    pivot_longer(
        cols = !(TableName:UNIT_MULT),
        names_to = "time",
        values_to = "dollars"
    ) |>
    mutate(year = str_extract(time, "[0-9]{4}") |> as.numeric()) |>
    select(LineDescription, year, dollars) |>
    left_join(FRED_data$CPI$annual |>
                  select(year, avgA),
              by = "year") |>
    pivot_wider(names_from = LineDescription, values_from = dollars) |>
    mutate(
        across(
            .cols = -c(year, avgA),
            .fns = function(x) {
                real_income <- x/avgA * 100
                log(real_income) - lag(log(real_income))
            }
        )
    ) |>
    select(-avgA) |>
    filter(!if_all(everything(-year), is.na))



    rename_with(
        .cols = -year,
        .fn = ~ .x |>
            gsub(" ", "_", .x) |>
            paste0(.x, "_PID")
    )



# Persional Income and Outlays Quarterly
pidQ_list <- append(nipaConfig,
                    list(
                        'datasetname' = 'NIPA',
                        'TableName' = 'T20100',
                        'Frequency' = 'Q'
                    ))

PID_Q <- beaGet(pidQ_list) |>
    pivot_longer(
        cols = !(TableName:UNIT_MULT),
        names_to = "time",
        values_to = "dollars"
    ) |>
    mutate(
        year = str_extract(time, "[0-9]{4}") |> as.numeric(),
        quarter = str_extract(time, "\\d$") |> as.numeric()
    ) |>
    select(LineDescription, year, quarter, dollars) |>
    pivot_wider(names_from = LineDescription, values_from = dollars) |>
    rename_with(~ gsub(" ", "_", .x)) |>
    rename_with(~ paste0(.x, "_GDP"), -c(year, quarter))


# Un-adjusted Data --------------------------------------------------------

PID_A_unadjusted <- beaGet(pidA_list) |>
    pivot_longer(
        cols = !(TableName:UNIT_MULT),
        names_to = "time",
        values_to = "dollars"
    ) |>
    mutate(year = str_extract(time, "[0-9]{4}") |> as.numeric()) |>
    select(LineDescription, year, dollars) |>
    pivot_wider(names_from = LineDescription, values_from = dollars) |>
    rename_with( ~ gsub(" ", "_", .x)) |>
    mutate(across(-year, ~ (.x - lag(.x))/lag(.x), .names = "{.col}_pct")) |>
    rename_with(~ paste0(.x, "_PID"), -year)
