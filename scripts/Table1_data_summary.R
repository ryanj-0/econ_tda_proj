#######################################################
## Table 1: Summary of data ranges, type, and frequency
#######################################################

data_summary_table <- all_annual |>
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
            series == "PID" ~ "Personal Income & Oulays",
            series == "FDI" ~ "Foreign Direct Investment",
            series == "FFR" ~ "Federal Funds Rate",
            series == "ECI" ~ "Employment Cost Index",
            series == "CPI" ~ "Consumer Price Index",
            series == "housingStarts" ~ "New Privately-Owned Housing Units Started",
            series == "PPI_All" ~ "Producer Price Index - All Commodities",
            series == "unemployment" ~ "Unemployment Rate"
        )
    ) |>
    mutate(
        data_frequency =  case_when(
            series == "GDP" ~ "Annual",
            series == "PID" ~ "Annual",
            series == "FDI" ~ "Annual",
            series == "FFR" ~ "Annual",
            series == "ECI" ~ "Quarterly",
            .default = "Monthly"
        )
    ) |>
    mutate(
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
        )
    ) |>
    mutate(
        API =  case_when(
            series == "GDP" ~ "BEA",
            series == "PID" ~ "BEA",
            series == "FDI" ~ "BEA",
            .default = "FRED"
        )
    ) |>
    relocate(series_name, .before = series) |>
    select(-series) |>
    gt() |>
    cols_align(align = "center",
               columns = c(year_start, year_end, API, data_frequency)
    ) |>
    cols_label(
        series_name = "Series Name",
        year_start = "Yeart Start",
        year_end = "Year End",
        data_frequency = "Data Frequency",
        data_type = "Data Type"
    ) |>
    opt_table_font(font = "EB Garamond",
                   size = 10.25) |>
    tab_options(
        page.margin.top = "top = 1in",
        page.margin.left =  "left = 1in",
        page.margin.bottom =  "bottom = 1in",
        page.margin.right = "right = 0.5in"
    )
