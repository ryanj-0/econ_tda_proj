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

e = 0.537

econ_bm <- BallMapper(points = pointcloud,
                      values = coloring,
                      epsilon = e)
econ_igraph <- bm_to_igraph(econ_bm)

# Old Graph
ColorIgraphPlot(outputFromBallMapper = econ_bm)
title(main = e)

# New Graph
bm_ggraph(econ_igraph, e)

investigate <- 1
reference_data[V(econ_igraph)$members[[investigate]], ]

# Parallel Epsilon Search -------------------------------------------------
library(future)
library(furrr)

epsilon_seq <- seq(0.4, 0.75, 0.001)

ballmapper_list <- function(ep) {

    gc()
    bm <- BallMapper(points = pointcloud, values = coloring, epsilon = ep)
    bm_final <- bm_to_igraph(bm)
    return(bm_final)

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
pdf(paste0("bm_loop_02_", format(Sys.Date(), "%Y%m%d"), ".pdf"))
walk2(
    .x = test,
    .y = epsilon_seq,
    .f = ~ {
        p <- new_bm_graph(.x, .y)
        print(p)
    }
)


dev.off()

