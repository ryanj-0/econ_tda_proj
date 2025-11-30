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

skeleton <- graph_from_edgelist(econ_bm$edges, directed = FALSE)
V(skeleton)$coloring <- econ_bm$coloring_values
V(skeleton)$node_size <- econ_bm$vertices[, "size"]

ggraph(skeleton, layout = "fr") +
    geom_edge_link(color = "grey80", width = 0.5) +
    geom_node_point(aes(color = coloring_values), size = node_size)

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

