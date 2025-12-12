#######################################################
## Table 2: Summary of data used in BallMapper Analysis
#######################################################

# Analysis Data Summary ---------------------------------------------------
analysis_data_summary <- analysisData |>
    map_df(
        .id = "series",
        ~ tibble(
            num_cols = ncol(.x) - 1
        )
    ) |>
    arrange(-num_cols) |>
    mutate(
        series_name =  case_when(
            series == "housingStarts" ~ "Housing Starts",
            series == "PPI_All" ~ "PPI",
            series == "unemployment" ~ "UnRate",
            .default = series
        ),
        economic_label = case_when(
            series == "PPI_All" ~
                "Supply-Side Signal",
            series == "PID" ~
                "Income Structure",
            series == "GDP" ~
                "Growth Composition",
            series == "CPI" ~
                "Demand-Side Signal",
            series == "unemployment" ~
                "Labor Market Dynamics",
            series == "FFR" ~
                "Monetary Policy",
            series == "housingStarts" ~
                "Leading Indicator"
        ),
        economic_description = case_when(
            series == "PPI_All" ~
                "Producer Costs & Measures Cost-Push Inflation",
            series == "PID" ~
                "Wages, Entrepreneurship, Gov. Transfers",
            series == "GDP" ~
                "Consumption, Investment, Gov., and Trade",
            series == "CPI" ~
                "Cost of Living & Measures Demand-Pull Inflation",
            series == "unemployment" ~
                "Labor Distress Level and Trend",
            series == "FFR" ~
                "Cost of Capital and Borrowing Conditions",
            series == "housingStarts" ~
                "Physical Residential Production"
        ),
        cyclical_timing = case_when(
            series %in% c("PPI_All", "housingStarts") ~ "Leading",
            series %in% c("unemployment", "CPI") ~ "Lagging",
            series %in% c("GDP", "PID") ~ "Concurrent",
            series == "FFR" ~ "Policy/Reactive"
        ),
        transformation = case_when(
            series %in% c("PPI_All", "CPI", "housingStarts") ~
                "12-month Avg. & Log Difference",
            series %in% c("FFR", "unemployment") ~
                "None (Annual Rate) & Simple Difference",
            series == "PID" ~ "Real Adjustment & Log Difference",
            series == "GDP" ~ "None (Source in % Change)"
        ),
        row_id = row_number()
    ) |>
    relocate(num_cols, .after = transformation) |>
    select(-series)

    # Analysis Data Summary Table -----------------------------------------
analysis_data_summary_table <- analysis_data_summary |>
    gt() |>
    cols_align(
        align = "center",
        columns = c(num_cols, cyclical_timing)
    ) |>
    cols_width(economic_description ~ px(130)) |>
    cols_label(
        series_name = "Data Series",
        economic_label = "Economic Role",
        economic_description = "Functional Descriptioin",
        transformation = "Transformation Applied",
        cyclical_timing = "Business Cycle Timing",
        num_cols = "Feature Count"
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
    cols_hide(columns = row_id) |>
    tab_options(
        table.width = pct(100),
        table.font.size = "8pt"
    ) |>
    opt_table_font(font = "EB Garamond")