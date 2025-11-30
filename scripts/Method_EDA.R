######################################################
## Exploratory Data Analysis for paper Methods section
######################################################

# Global Data -------------------------------------------------------------

reference_data <- finalData |>
    mutate(row_id = row_number())


pointcloud <- reference_data |>
    select(-c(year, row_id)) |>
    as.data.frame() |>
    normalize_to_min_0_max_1()

coloring <- finalData |>
    select(year) |>
    as.data.frame()


# BallMapper Testing ------------------------------------------------------

e = 0.6

econ_bm <- BallMapper(points = pointcloud, values = coloring, epsilon = e)

ColorIgraphPlot(outputFromBallMapper = econ_bm)
title(main = e)


# Custom Graph ------------------------------------------------------------

# Prep Data for Graph Transormation
nodes_df <- econ_bm$vertices |> as.data.frame()
edges_df <- econ_bm$edges |> as.data.frame()

# Create Basic Graph
skeleton <- graph_from_data_frame(
    d = edges_df,
    vertices = vertices_df,
    directed = FALSE
)

# Adding Additional Outputs from BM
V(skeleton)$coloring_values <- econ_bm$coloring
V(skeleton)$rows_covered <- econ_bm$points_covered_by_landmarks

# Plot New Graph
ggraph(skeleton, layout = "fr") +
    geom_edge_link(
        color = "#BEBEBE",
        width = 0.5
    ) +
    geom_node_point(
        aes(
            color = coloring_values,
            size = size
        )
    ) +
    geom_node_text(
        aes(label = name)
    ) +
    scale_size_area(
        max_size = 25,
        guide = "none"
    ) +
    scale_color_gradientn(
        name = coloring |> names() |> str_to_title(),
        colors = c("#67A9CF", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#EF8A62")
    ) +
    theme_void() +
    labs(
        title = "BallMapper Output",
        subtitle = paste0("Epsilon: ", e),
        caption = paste0("N: ", nrow(pointcloud))
    ) +
    theme(
        text = element_text(family = "EB Garamond"),
        legend.position = "bottom",
        legend.key.width = unit(0.1, "npc")

    )

# Node Investigation ------------------------------------------------------
node <- 26

interest <- points_covered_by_landmarks(outputFromBallMapper = econ_bm,
                                        numbers_of_landmarks = node)
investigate <- reference_data[interest, ]
investigate |> glimpse()
investigate |> view()



# Parallel Epsilon Search -------------------------------------------------
library(future)
library(furrr)

epsilon_seq <- seq(0.3, 1.4, 0.01)

ballmapper_list <- function(ep) {

    gc()
    BallMapper(points = pointcloud, values = coloring, epsilon = ep)

}


# computing
plan(strategy = multisession,
     workers = parallel::detectCores() - 1)

test <- future_map(
    .x = epsilon_seq,
    .f = ballmapper_list,
    .options = furrr_options(seed = TRUE))

# parallel off
plan(sequential)

# plot all bm graphs
pdf("bm_loop.pdf")
walk2(
    .x = test,
    .y = epsilon_seq,
    .f = function(.x, .y) {
        ColorIgraphPlot(outputFromBallMapper = .x)
        title(main = .y)
    }
)


dev.off()

