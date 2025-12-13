#########################################
# Exploratory Script
#########################################

# Parallel Epsilon Search -------------------------------------------------
library(future)
library(furrr)

exploratory_bm <- function(ep) {

    explore_coloring <- final_data |>
        select(Year) |>
        as.data.frame()

    explore_bm <- BallMapper(points = pointcloud,
                     values = explore_coloring,
                     epsilon = ep)
    explore_final <- bm_to_igraph(explore_bm)
    return(explore_final)

}

# computing
epsilon_seq <- seq(0.5, 0.8, 0.01)

# set workers
if(Sys.info()[["nodename"]] == "zenbook") {
    plan(strategy = multisession,
         workers = parallel::detectCores() - 1)
} else {
    plan(strategy = multisession,
         workers = parallel::detectCores() - 2)
}

# run parallel
explore_list <- future_map(
    .x = epsilon_seq,
    .f = exploratory_bm,
    .options = furrr_options(seed = TRUE)
)

# parallel off
plan(sequential)

gc()

# plot all bm graphs
pdf(paste0(format(Sys.Date(), "%Y%m%d"), "_explore_loop.pdf"))
walk2(
    .x = explore_list,
    .y = epsilon_seq,
    .f = ~ {
        p <- bm_ggraph(.x, .y)
        print(p)
    }
)

dev.off()

# Further Analysis With Chosen Epsilon ------------------------------------

coloring_vec <- final_data |>
    select(-c(row_id)) |>
    names()

e = 0.511

# Computing All Colorings
library(future)
library(furrr)

econ_all_coloring <- function(coloring) {

    # Set coloring
    coloring <- final_data |>
        select(all_of(coloring)) |>
        as.data.frame()

    # Run BallMapper
    bm <- BallMapper(points = pointcloud, values = coloring, epsilon = e)
    bm_final <- bm_to_igraph(bm)
    return(bm_final)

}

# computing
if(Sys.info()[["nodename"]] == "zenbook") {
    plan(strategy = multisession,
         workers = parallel::detectCores() - 1)
} else {
    plan(strategy = multisession,
         workers = parallel::detectCores() - 2)
}


econ_all_bm <- future_map(
    .x = coloring_vec,
    .f = econ_all_coloring,
    .options = furrr_options(seed = 2025))

# parallel off
plan(sequential)

gc()

# plot all bm graphs
pdf(paste0("bm_all_coloring_00_", format(Sys.Date(), "%Y%m%d"), ".pdf"))
walk2(
    .x = test,
    .y = coloring_vec,
    .f = ~ {
        p <- bm_ggraph(.x, .y)
        print(p)
    }
)

dev.off()