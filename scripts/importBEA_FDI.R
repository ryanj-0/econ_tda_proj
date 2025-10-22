########################################################
## Loading Data From Bureau of Economic Analysis (BEA):
## Direct Investment and Multinational Enterprises (MNE)
## - Foriegn Direct Investment
#######################################################

FDI_A <- beaGet(
    list(
        'UserID' = my_beaKey,
        'Method' = 'GetData',
        'datasetname' = 'MNE',
        'DirectionOfInvestment' = 'Inward',
        'Classification' = 'Industry',
        'Year' = "All"
    )
) |>
    filter(
        Column %in% c("First-year expenditures", "Planned total expenditures"),
        Row == "All Industries Total"
    ) |>
    pivot_longer(
        cols = !SeriesID:TableRowDisplayOrder,
        names_to = "time",
        values_to = "expenditures"
        ) |>
    mutate(year = str_extract(time, "[0-9]{4}")) |>
    drop_na(expenditures) |>
    select(Row, Column, TableScale, year, expenditures)
