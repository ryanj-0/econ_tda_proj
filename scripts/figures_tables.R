##############################################
## Figures and Tables
##############################################


# Figure 1 ----------------------------------------------------------------

## The Data
yearSpan <- map(all_annual, ~ .x$year |> unique())

yearRanges <- map_df(
    yearSpan,
    ~ tibble(
        start_year = min(.x, na.rm = TRUE),
        end_year = max(.x, na.rm = TRUE)
    ),
    .id = "data_source"
) |>
    mutate(
        data_source = case_when(
            data_source == "housingStarts" ~ "Housing Starts",
            data_source == "unemployment" ~ "Unemployment",
            .default = data_source
        ),
        data_source = fct_reorder(data_source, start_year, .desc = TRUE),
        startLabel = if_else(
            start_year %% 25 != 0,
            paste0("'", str_sub(start_year, 3, 4)),
            NA_character_
        ),
        endLabel = if_else(
            end_year %% 25 != 0,
            paste0("'", str_sub(end_year, 3, 4)),
            NA_character_
        ),
        roundStart = if_else(
            start_year %% 25 == 0,
            paste0("'", str_sub(start_year, 3, 4)),
            NA_character_
        ),
        roundEnd = if_else(
            end_year %% 25 == 0,
            paste0("'", str_sub(end_year, 3, 4)),
            NA_character_
        )
    )

## The Plot
yearSpan_plot <- ggplot(yearRanges, aes(y = data_source)) +
    geom_segment(
        aes(x = start_year, xend = end_year),
        linewidth = 1,
        color = "steelblue"
        )+
    geom_point(aes(x = start_year), size = 2, color = "grey20") +
    geom_point(aes(x = end_year), size = 2, color = "grey20") +
    labs(x = "Year", y = "Data Source") +
    # left side not round dates
    geom_text(
        aes(x = start_year, label = startLabel),
        size = 3,
        hjust = 1,
        vjust = 0,
        nudge_y = -0.5
    ) +
    # left side round dates
    geom_text(
        aes(x = start_year, label = roundStart),
        size = 3,
        hjust = 1,
        vjust = 0,
        nudge_y = -0.5,
        alpha = 0.5
    ) +
    #right side not round dates
    geom_text(
        aes(x = end_year, label = endLabel),
        size = 3,
        hjust = 1,
        vjust = 0,
        nudge_y = -0.5
    ) +
    # right side round dates
    geom_text(
        aes(x = end_year, label = roundEnd),
        size = 3,
        hjust = 1,
        vjust = 0,
        nudge_y = -0.5,
        alpha = 0.5
    ) +
    theme_bw() +
    theme(
        text = element_text(family = "EB Garamond"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))
    )

