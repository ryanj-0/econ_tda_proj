##########################################################
## Table 4: Summary of data to Review Ball Mapper Analysis
##########################################################

# Review Data Table ---------------------------------------------------
node_data <- names(final_data) |>
    str_replace_all("_", " ") |>
    as_tibble() |>
    filter(value != "Private Public Driver") |>
    mutate(
        value = str_replace(value, "Fisher Equation", "Real Interest Rate"),
        value = str_replace(value, "recession span", "Recession Span (0-1 by 0.25)"),
        value = str_replace(value, "expansion span", "Expansion Span (0-1 by 0.25)"),
        value = str_replace(value, "Wage Growth", "Real Wage Growth"),
        value = str_replace(value, "row id", "Row Number")
    ) |>
    mutate(
        row = rep(1:6, each = 4),
        col = rep(1:4, times = 6)
    ) |>
    pivot_wider(
        names_from = col,
        values_from = value,
        names_prefix = "V"
    )


# Analysis Data Summary Table -----------------------------------------
node_data_table <- node_data |>
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
        locations = cells_body(rows = seq(2, nrow(node_data), 2))
    ) |>
    tab_options(
        table.width = pct(100),
        table.font.size = "8pt"
    ) |>
    opt_table_font(font = "EB Garamond")
