######################################################
## Exploratory Data Analysis for paper Methods section
######################################################

# Global Data -------------------------------------------------------------

coloring <- final_data |>
    select(Fisher_Equation) |>
    as.data.frame()
e <- 0.8


# Investigation
bm <- BallMapper(points = pointcloud, values = coloring, epsilon = e)
bm_final <- bm_to_igraph(bm)

# Testing calculation for igraph
V(bm_final)$degree <- degree(bm_final)

# test graph
source(paste(getwd(), "functions/test_ggraph.R", sep = "/"))
test_ggraph(bm_final, coloring = coloring, epsilon = e)

# investigate nodes
node <- c(19:23)
final_data[V(bm_final)$members[node] |> unlist(), ]

# test dual graph
comparision_map <- c("Year", "Unemployment")
test <- map(.x = comparision_map,
            .f = ~ {

                # Coloring
                coloring_test <- final_data |>
                    select(all_of(.x)) |>
                    as.data.frame()

                # Run BallMapper
                bm_test <- BallMapper(points = pointcloud,
                                      values = coloring_test,
                                      epsilon = 0.511)
                bm_igraph <- bm_to_igraph(bm_test)

                # ggGraph
                test_ggraph(bm_igraph,
                            coloring = coloring_test,
                            epsilon = 0.511)
            })

wrap_plots(test, ncol = 2)


# GDP Maps ----------------------------------------------------------------

gdp_vars <- c("Consumption", "Domestic_Investment", "Government_Spending",
              "Exports", "Imports")
gdp_maps <- map(.x = gdp_vars,
            .f = ~ {

                # Coloring
                gdp_coloring <- final_data |>
                    select(all_of(.x)) |>
                    as.data.frame()

                # Run BallMapper
                gdp_bm <- BallMapper(points = pointcloud,
                                      values = gdp_coloring,
                                      epsilon = 0.511)
                gdp_igraph <- bm_to_igraph(gdp_bm)

                # ggGraph
                test_ggraph(gdp_igraph,
                            coloring = gdp_coloring,
                            epsilon = 0.511)
            })

custom_layout <- "
    AABBCC
    #DDEE#
"

wrap_plots(gdp_maps, design = custom_layout)


# Correlation Table -------------------------------------------------------

pointcloud |>
    correlate() |>
    shave() |>
    rplot(
        print_cor = TRUE,
        colors = c("#0072B2", "#FFFFFF", "#E69F00")
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

