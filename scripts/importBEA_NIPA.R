########################################################
## Loading Data From Beureau of Economic Analysis (BEA):
## National Income and Product Accounts (NIPA)
## - GDP
########################################################

# Global config for NIPA Tables
nipaConfig <- list('UserID' = my_beaKey,
                  'Method' = 'GetData',
                  'Year' = 'ALL',    # Get All Years Available
                  'ResultFormat' = 'json')


# GDP ---------------------------------------------------------------------

# Annual GDP
gdpA_list <- append(beaConfig, list('datasetname' = 'NIPA',
                                    'TableName' = 'T10101',
                                    'Frequency' = 'A'))

GDP_A <- beaGet(gdpA_list) |>
    pivot_longer(cols = !(TableName:UNIT_MULT),
                 names_to = "time",
                 values_to = "pctChange") |>
    mutate(year = str_extract(time, "[0-9]{4}"),
           LineDescription = case_when(LineNumber == 17 ~ "ExportGoods",
                                       LineNumber == 18 ~ "ExportServices",
                                       LineNumber == 20 ~ "ImportGoods",
                                       LineNumber == 21 ~ "ImportServices",
                                       .default = LineDescription)) |>
    select(LineDescription, CL_UNIT, year, pctChange) |>
    pivot_wider(names_from = LineDescription,
                values_from = pctChange)

# GDP Quartly
gdpQ_list <- append(beaConfig, list('datasetname' = 'NIPA',
                                    'TableName' = 'T10101',
                                    'Frequency' = 'Q'))
GDP_Q <- beaGet(gdpQ_list) |>
    pivot_longer(cols = !(TableName:UNIT_MULT),
                 names_to = "time",
                 values_to = "pctChange") |>
    mutate(year = str_extract(time, "[0-9]{4}",),
           quarter = str_extract(time, "\\d$"),
           LineDescription = case_when(LineNumber == 17 ~ "ExportGoods",
                                       LineNumber == 18 ~ "ExportServices",
                                       LineNumber == 20 ~ "ImportGoods",
                                       LineNumber == 21 ~ "ImportServices",
                                       .default = LineDescription)) |>
    select(LineDescription, CL_UNIT, year, quarter, pctChange) |>
    pivot_wider(names_from = LineDescription,
                values_from = pctChange)


# Personal Income ---------------------------------------------------------
pidaA_list <- append(beaConfig, list('datasetname' = 'NIPA',
                                     'TableName' = 'T20100',
                                     'Frequency' = 'A'))
pidaA <- beaGet(pidaA_list) |>
    pivot_longer(cols = !(TableName:UNIT_MULT),
                 names_to = "time",
                 values_to = "dollars") |>
    mutate(year = str_extract(time, "[0-9]{4}"))

