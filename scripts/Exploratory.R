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