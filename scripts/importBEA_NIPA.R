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
        year = str_extract(time, "[0-9]{4}"),
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
    rename_with( ~ gsub(" ", "_", .x)) |>
    rename_with( ~ paste0(.x, "_%"), .cols = -year)

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
        year = str_extract(time, "[0-9]{4}", ),
        quarter = str_extract(time, "\\d$"),
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
    rename_with( ~ gsub(" ", "_", .x)) |>
    rename_with( ~ paste0(.x, "_%"), .cols = -c(year, quarter))


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
    mutate(year = str_extract(time, "[0-9]{4}")) |>
    select(LineDescription, year, dollars) |>
    pivot_wider(names_from = LineDescription, values_from = dollars) |>
    rename_with( ~ gsub(" ", "_", .x)) |>
    rename_with( ~ paste0(.x, "_M"), .cols = -year)


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
    mutate(year = str_extract(time, "[0-9]{4}"),
           quarter = str_extract(time, "\\d$")) |>
    select(LineDescription, year, quarter, dollars) |>
    pivot_wider(names_from = LineDescription, values_from = dollars) |>
    rename_with( ~ gsub(" ", "_", .x)) |>
    rename_with( ~ paste0(.x, "_M"), .cols = -c(year, quarter))
