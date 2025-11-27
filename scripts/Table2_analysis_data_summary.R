#######################################################
## Table 2: Summary of data used in BallMapper Analysis
#######################################################

analysis_data_summary_table <- analysisData |>
    map_df(
        .id = "series",
        ~ tibble(
            num_cols = ncol(.x) - 1
        )
    ) |>
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
        )
    ) |>
    relocate(series_name, .before = series) |>
    relocate(c(economic_label, economic_description), .after = series_name) |>
    select(-series) |>
    gt() |>
    cols_align(align = "center",
               columns = c(
                   num_cols
                   )
    ) |>
    cols_width(economic_label ~ px(150),
               economic_description ~ px(100)) |>
    cols_label(
        series_name = "Data Series",
        economic_label = "Economic Role",
        economic_description = "Functional Descriptioin",
        num_cols = "Feature Count"
    ) |>
    opt_table_font(font = "EB Garamond",
                   size = 10.25) |>
    tab_options(
        page.margin.top = "top = 1in",
        page.margin.left =  "left = 1in",
        page.margin.bottom =  "bottom = 1in",
        page.margin.right = "right = 0.5in"
    )
