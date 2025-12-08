######################################################
## Exploratory Data Analysis for paper Methods section
######################################################

# Global Data -------------------------------------------------------------

coloring <- final_data |>
    select(year) |>
    as.data.frame()

# Investigation
bm <- BallMapper(points = pointcloud, values = coloring, epsilon = 0.511)
bm_final <- bm_to_igraph(bm)

# Testing calculation for igraph
V(bm_final)$degree <- degree(bm_final)


# test graph
source(paste(getwd(), "functions/test_ggraph.R", sep = "/"))
test_ggraph(bm_final, coloring = coloring |> names(), epsilon = 0.511)

# investigate nodes
node <- 4
final_data[V(bm_final)$members[[node]], ] |> summary() |> view()

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


# Correlation Table -------------------------------------------------------

pointcloud |>
    correlate() |>
    shave() |>
    rplot(shape = 15, print_cor = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
