changeTOAnnually <- function(df) {
    return_df <- df |>
        mutate(year = str_extract(date, "[0-9]{4}") |> as.numeric()) |>
        group_by(year) |>
        mutate(avgA = mean(value)) |>
        ungroup() |>
        select(series_id, year, avgA) |>
        unique()

    return(return_df)
}