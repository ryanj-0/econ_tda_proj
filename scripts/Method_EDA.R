######################################################
## Exploratory Data Analysis for paper Methods section
######################################################

# Global Data -------------------------------------------------------------

reference_data <- finalData |>
    mutate(row_id = row_number()) |>
    left_join(nberRecessions_yearly |> select(year, fullYear)) |>
    mutate(recession = if_else(is.na(fullYear), 0, 1)) |>
    relocate(recession, .after = year)


pointcloud <- reference_data |>
    select(-c(year, row_id, fullYear, recession)) |>
    as.data.frame() |>
    normalize_to_min_0_max_1()

coloring <- finalData |>
    select(year) |>
    as.data.frame()


# BallMapper Testing ------------------------------------------------------

e = 0.511

econ_bm <- BallMapper(points = pointcloud,
                      values = coloring,
                      epsilon = e)
econ_igraph <- bm_to_igraph(econ_bm)

# Old Graph
ColorIgraphPlot(outputFromBallMapper = econ_bm)
title(main = e)

# New Graph
bm_ggraph(econ_igraph, e)

components(econ_igraph)

investigate <- 26
see <- reference_data[V(econ_igraph)$members[[investigate]], ]
see

# Parallel Epsilon Search -------------------------------------------------
library(future)
library(furrr)

epsilon_seq <- c(0.478, 0.511, 0.600, 0.605)

ballmapper_list <- function(ep) {

    bm <- BallMapper(points = pointcloud, values = coloring, epsilon = ep)
    bm_final <- bm_to_igraph(bm)
    return(bm_final)

}


# computing
plan(strategy = multisession,
     workers = parallel::detectCores() - 2)

test <- future_map(
    .x = epsilon_seq,
    .f = ballmapper_list,
    .options = furrr_options(seed = TRUE))

# parallel off
plan(sequential)

gc()

# plot all bm graphs
pdf(paste0("bm_loop_04_", format(Sys.Date(), "%Y%m%d"), ".pdf"))
walk2(
    .x = test,
    .y = epsilon_seq,
    .f = ~ {
        p <- bm_ggraph(.x, .y)
        print(p)
    }
)

dev.off()

