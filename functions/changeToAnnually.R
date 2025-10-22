changeTOAnnually <- function(df) {
    return_df <- df |>
        group_by(year) |>
        mutate(avgA = mean(avgQ)) |>
        ungroup() |>
        select(series_id, year, avgA) |>
        unique()

    return(return_df)
}