changeToQuarterly <- function(df) {
    return_df <- df |>
        mutate(
            year = format(date, "%Y") |> as.numeric(),
            month = format(date, "%m") |> as.numeric(),
            quarter = case_when(
                month %in% c(1:3) ~ 1,
                month %in% c(4:6) ~ 2,
                month %in% c(7:9) ~ 3,
                month %in% c(10:12) ~ 4
            ),
            yearQuarter = paste0(year, quarter)
        ) |> # need a unique group_by
        group_by(yearQuarter) |>
        mutate(avgQ = mean(value)) |>
        ungroup() |>
        select(series_id, year, quarter, avgQ) |>
        unique()

    return(return_df)
}