#######################################################
## Table 1: Summary of data ranges, type, and frequency
#######################################################

# Data for Summary Table --------------------------------------------------
data_summary <- all_annual |>
    map_df(
        .id = "series",
        ~ tibble(
            year_start = min(.x$year),
            year_end = max(.x$year),
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
        ),
        series_abbreviation =  case_when(
            series == "housingStarts" ~ "Housing Starts",
            series == "PPI_All" ~ "PPI",
            series == "unemployment" ~ "Unrate",
            .default = series
        ),
        data_frequency =  case_when(
            series == "GDP" ~ "Annual",
            series == "PID" ~ "Annual",
            series == "FDI" ~ "Annual",
            series == "FFR" ~ "Annual",
            series == "ECI" ~ "Quarterly",
            .default = "Monthly"
        ),
        data_type =  case_when(
            series == "GDP" ~ "Flow",
            series == "PID" ~ "Flow",
            series == "FDI" ~ "Flow",
            series == "FFR" ~ "Rate",
            series == "ECI" ~ "Index",
            series == "CPI" ~ "Index",
            series == "housingStarts" ~ "Flow*",
            series == "PPI_All" ~ "Index",
            series == "unemployment" ~ "Rate"
        ),
        API =  case_when(
            series == "GDP" ~ "BEA",
            series == "PID" ~ "BEA",
            series == "FDI" ~ "BEA",
            .default = "FRED"
        ),
        row_id = row_number()
    ) |>
    relocate(c(series_name, series_abbreviation), .before = series) |>
    select(-series)


# Summary Table -----------------------------------------------------------
data_summary_table <- data_summary |>
    gt() |>
    cols_align(
        align = "center",
        columns = c(
            series_abbreviation,
            year_start,
            year_end,
            data_type,
            data_frequency,
            API
        )
    ) |>
    cols_width(series_abbreviation ~ px(150)) |>
    cols_label(
        series_name = "Data Series",
        series_abbreviation = "Series Abbreviation",
        year_start = "Year Start",
        year_end = "Year End",
        data_frequency = "Data Frequency",
        data_type = "Data Type"
    ) |>
    tab_style(
        style = list(
            cell_fill(color = "#F2F2F2"),
            cell_borders(
                sides = c("left", "right"),
                color = "#F2F2F2",
                weight = px(12)
            )
        ),
        locations = list(
            cells_body(rows = row_id %% 2 == 0),
            cells_stub(rows = row_id %% 2 == 0)
        )
    ) |>
    cols_hide(columns = row_id) |>
    tab_options(
        table.width = pct(100),
        table.font.size = "8pt"
    ) |>
    opt_table_font(font = "EB Garamond")
