#########################################
# Exploratory Script
#########################################

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
pdf(paste0("bm_loop_00_", format(Sys.Date(), "%Y%m%d"), ".pdf"))
walk2(
    .x = test,
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