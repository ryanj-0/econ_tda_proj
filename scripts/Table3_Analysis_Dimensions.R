#######################################################
## Table 3: Data used in Ball Mapper Analysis
#######################################################

# Analysis Data Summary ---------------------------------------------------
analysis_dimensions <- names(pointcloud) |>
    str_replace_all("_", " ") |>
    as_tibble() |>
    mutate(
        row = rep(1:4, each = 4),
        col = rep(1:4, times = 4)
    ) |>
    pivot_wider(
        names_from = col,
        values_from = value,
        names_prefix = "V"
    )


    # Analysis Data Summary Table -----------------------------------------
analysis_dimensions_table <- analysis_dimensions |>
    gt() |>
    cols_hide(columns = row) |>
    cols_label(
        V1 = "",
        V2 = "",
        V3 = "",
        V4 = ""
    ) |>
    tab_style(
        style = cell_fill(color = "#F2F2F2"),
        locations = cells_body(rows = seq(2, nrow(analysis_dimensions), 2))
    ) |>
    tab_options(
        table.width = pct(100),
        table.font.size = "8pt"
    ) |>
    opt_table_font(font = "EB Garamond")
