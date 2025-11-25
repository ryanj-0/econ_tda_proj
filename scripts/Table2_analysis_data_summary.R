#######################################################
## Table 2: Summary of data used in BallMapper Analysis
#######################################################

analysis_data_summary_table <- analysisData |>
    map_df(
        .id = "series",
        ~ tibble(
            num_cols = ncol(.x)
        )
    ) |>
    mutate(
        series_name =  case_when(
            series == "PPI_All" ~ "Producer Price Index - All Commodities",
            series == "PID" ~ "Personal Income & Oulays",
            series == "GDP" ~ "Gross Domestic Product",
            series == "CPI" ~ "Consumer Price Index",
            series == "unemployment" ~ "Unemployment Rate",
            series == "FFR" ~ "Federal Funds Rate",
            series == "housingStarts" ~ "New Privately-Owned Housing Units Started"
        )
    ) |>
    mutate(
        inclusion = case_when(
            series == "PPI_All" ~
                "Supply-Side Signal & Cost-Push Inflation",
            series == "PID" ~
                "Pop. Earnings, Entreprenurship, Gov. Transfer Progs.",
            series == "GDP" ~
                "Components of Economic Growth",
            series == "CPI" ~ "Demand side of economy, consumers; demand-pull inflation",
            series == "unemployment" ~ "Measure recessionary/depressionary pressures.",
            series == "FFR" ~ "Basis for the cost of capital.",
            series == "housingStarts" ~ "Rate of housing being built."
        )
    ) |>
    relocate(series_name, .before = series) |>
    relocate(inclusion, .after = series_name) |>
    select(-series) |>
    gt() |>
    cols_align(align = "center",
               columns = c(
                   num_cols
                   )
    ) |>
    cols_width(inclusion ~ px(300)) |>
    cols_label(
        series_name = "Series Name",
        num_cols = "Num. of Cols."
    ) |>
    opt_table_font(font = "EB Garamond",
                   size = 10.25) |>
    tab_options(
        page.margin.top = "top = 1in",
        page.margin.left =  "left = 1in",
        page.margin.bottom =  "bottom = 1in",
        page.margin.right = "right = 0.5in"
    )
