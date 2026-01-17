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

    explore_bm <- BallMapper(points = test_pointcloud,
                     values = explore_coloring,
                     epsilon = ep)
    explore_final <- bm_to_igraph(explore_bm)
    return(explore_final)

}

# computing
epsilon_seq <- c(0.474, 0.476, 0.488, 0.488, 0.514, 0.521, 0.570, 0.606)

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
explore_coloring <- final_data |>
    select(Year) |>
    as.data.frame()
pdf(paste0(format(Sys.Date(), "%Y%m%d"), "_explore_loop.pdf"))
walk2(
    .x = explore_list,
    .y = epsilon_seq,
    .f = ~ {
        p <- bm_ggraph(bm_igraph = .x,
                       coloring = explore_coloring,
                       epsilon = .y)
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



# Notes -------------------------------------------------------------------

# 11/29/20205
# - inital range 0.3 - 1.4
# - ran seq to find interesting maps (bm_loop_00), found that .4-.75 seem to produce the most interesting graphs so we will run another sequence to see if there are any nuances we should be interested in (bm_loop_01)
# - starting at 0.0447, maps have 3 components
# - around 0.468, small components start to have more points included. We see this reflected in sizing between connected points
#
# 11/30/2025
# - run new maps with new graph function
# - 0.537 & 0.538 have a three node component?
#     - 0.546 & 0.547 have 4 components?
#     - 0.61-0.625 2 components but the shape changes b/c of points moving around
# - 0.677-0.75 2 components and various combine of nodes
# - Most Interesting Epsilon Values:
#     -
#
#     12/1/2025
# - Choosing handful of maps to see which to use for analysis
# - c(0.468, 0.473, 0.478, 0.487, 0.511, 0.600, 0.605, 0.614, 0.642, 0.716)
# - 0.475-0.482 stable graph, no change
# - 0.495 start to see 2-simplex form, persists throughout
# - 3-7-11 --> 3-6-10 -->
#     - Noticing developing three big groups of nodes, three "phases" of economy?
#     - talk about differnt shapes: 2-simplex, mickey mouse, spurs, lolipop holders,
# - make edge strenth acording edge length? #discussion
#     - later year v past year 2-simplex pattern?
#
#     12/2/2025
# - Narrowing down more which graph for analysis
