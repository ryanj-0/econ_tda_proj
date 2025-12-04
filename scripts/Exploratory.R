#########################################
# Exploratory Script
#########################################

# Test ggraph -------------------------------------------------------------

test_ggraph <- function(bm_igraph_output, coloring, epsilon) {

    ggraph(bm_igraph_output, layout = "kk") +
        geom_edge_link(
            aes(width = weight,
                color = weight > mean(weight)
            )
        ) +
        scale_edge_color_manual(
            values = c("TRUE" = "#000000",
                       "FALSE" = "#808080"),
            guide = "none"
        ) +
        scale_edge_width(
            range = c(0.2, 1),
            guide = "none"
        ) +
        geom_node_point(
            aes(
                fill = coloring,
                size = size
            ),
            shape = 21,
            color = "#000000",
            stroke = 0.5
        ) +
        geom_node_text(
            aes(label = name
            )
        ) +
        scale_size_area(
            max_size = 25,
            guide = "none"
        ) +
        scale_fill_gradientn(
            name = coloring |> str_to_title(),
            colors = c("#0072B2", "#00B0E1", "#009B77", "#F0E442", "#E69F00"),
            guide = guide_colorbar(
                title.position = "bottom",
                title.hjust = 0.5,
                title.vjust = 1,
                label.position = "top"
            )
        ) +
        labs(
            title = paste("BallMapper Output | ", str_to_title(coloring)),
            subtitle = paste0("Epsilon: ", epsilon, " | N: ", nrow(pointcloud))
        ) +
        theme_void() +
        theme(
            text = element_text(family = "EB Garamond"),
            legend.position = "bottom",
            legend.key.width = unit(0.1, "npc")

        )
}



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