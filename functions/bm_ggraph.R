############################################
# Function to take BallMapper output and
# graph it in a ggraph output for
# easier readability.
############################################

bm_ggraph <- function(bm_igraph_output, coloring, epsilon) {

    # Set graph details
    coloring_name <- coloring |>
        names() |>
        str_replace_all("_", " ")
    num_nodes <- vcount(bm_igraph_output)

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
            name = coloring_name,
            colors = c("#0072B2", "#66C2A5", "#AA3377", "#F0E442", "#FF9F1C"),
            guide = guide_colorbar(
                title.position = "bottom",
                title.hjust = 0.5,
                title.vjust = 1,
                label.position = "top"
            )
        ) +
        theme_void() +
        theme(
            text = element_text(family = "EB Garamond"),
            legend.position = "bottom",
            legend.key.width = unit(0.1, "npc")
        )
}