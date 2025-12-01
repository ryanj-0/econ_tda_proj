############################################
# Function to take BallMapper output and
# graph it in a ggraph output for
# easier readability.
############################################

bm_ggraph <- function(bm_igraph_output, epsilon) {

    ggraph(bm_igraph_output, layout = "kk") +
        geom_edge_link(
            aes(width = weight,
                color = weight > mean(weight)
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
            aes(label = name,
                size = log(size))
        ) +
        scale_size_area(
            max_size = 25,
            guide = "none"
        ) +
        scale_color_gradientn(
            name = coloring |> names() |> str_to_title(),
            colors = c("#67A9CF", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#EF8A62")
        ) +
        labs(
            title = "BallMapper Output",
            subtitle = paste0("Epsilon: ", epsilon," | N: ", nrow(pointcloud))
        ) +
        theme_void() +
        theme(
            text = element_text(family = "EB Garamond"),
            legend.position = "bottom",
            legend.key.width = unit(0.1, "npc"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 18)

        )
}