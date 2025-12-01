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


econ_igraph <- bm_to_igraph(econ_bm)

investigate <- 17
reference_data[V(econ_igraph)$members[[investigate]], ]

# Custom Graph ------------------------------------------------------------

new_bm_graph <- function(bm_igraph_output, epsilon) {

    ggraph(bm_igraph_output, layout = "kk") +
        geom_edge_link(
            aes(width = weight,
                color = weight > 3
            )
        ) +
        scale_edge_color_manual(
            values = c("TRUE" = "#000000",
                       "FALSE" = "#BEBEBE"),
            guide = "none"
        ) +
        scale_edge_width(
            range = c(0.2, 1),
            guide = "none"
        ) +
        geom_node_point(
            aes(
                color = coloring,
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
            subtitle = paste0("Epsilon: ", epsilon),
            caption = paste0("N: ", nrow(pointcloud))
        ) +
        theme(
            text = element_text(family = "EB Garamond"),
            legend.position = "bottom",
            legend.key.width = unit(0.1, "npc")

        )
}

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

