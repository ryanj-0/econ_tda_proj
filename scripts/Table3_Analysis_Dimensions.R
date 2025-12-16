#######################################################
## Table 2: Summary of data used in BallMapper Analysis
#######################################################

# Analysis Data Summary ---------------------------------------------------
analysis_dimensions <- analysisData |>
    map_df(
        .id = "series",
        ~ tibble(
            num_cols = ncol(.x) - 1,
            long_name = names(.x)
        )
    ) |>
    mutate(
        series_name =  case_when(
            series == "GDP" ~ "Gross Domestic Product",
            series == "PID" ~ "Personal Income & Its Disposition",
            series == "FDI" ~ "Foreign Direct Investment",
            series == "FFR" ~ "Federal Funds Rate",
            series == "ECI" ~ "Employment Cost Index",
            series == "CPI" ~ "Consumer Price Index",
            series == "housingStarts" ~ "New Privately-Owned Housing Units Started",
            series == "PPI_All" ~ "Producer Price Index - All Commodities",
            series == "unemployment" ~ "Unemployment Rate"
        )
    ) |>
    mutate(across(series_name, ~ ifelse(long_name == "year", "All", .x))) |>
    mutate(across(long_name, ~ str_remove(.x, "_[^_]+$"))) |>
    mutate(across(long_name, ~ str_replace_all(.x, "_", " "))) |>
    mutate(across(long_name, ~ str_to_title(.x))) |>
    select(-c(series, num_cols)) |>
    unique() |>
    filter(long_name != "Gross Domestic Product") |>
    cbind(short_name = c("Year", names(pointcloud))) |>
    mutate(across(short_name, ~ str_replace_all(.x, "_", " "))) |>
    mutate(row_id = row_number())


    # Analysis Data Summary Table -----------------------------------------
analysis_dimensions_table <- analysis_dimensions |>
    gt() |>
    cols_label(
        series_name = "Data Series",
        long_name = "Official Measure",
        short_name = "Analysis Name"
    ) |>
    tab_style(
        style = list(
            cell_fill(color = "#F2F2F2"),
            cell_borders(
                sides = c("left", "right"),
                color = "#F2F2F2",
                weight = px(1)
            )
        ),
        locations = list(
            cells_body(rows = row_id %% 2 == 0),
            cells_stub(rows = row_id %% 2 == 0)
        )
    ) |>
    cols_hide(columns = c(long_name, row_id)) |>
    tab_options(
        table.width = pct(100),
        table.font.size = "8pt"
    ) |>
    opt_table_font(font = "EB Garamond")
