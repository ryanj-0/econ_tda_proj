#####################################################
## Function to transform BallMapper ouput funciton
## BallMapper::BallMapper() to igraph object for
## easier use in ggraph.
#####################################################

bm_to_igraph <- function(ballmapper_output) {

    # --- BUILD BASIC GRAPH ---

    # Extract Nodes and Edges
    nodes_df <- ballmapper_output$vertices |> as.data.frame()
    colnames(nodes_df) <- c("id", "size")

    edges_df <- ballmapper_output$edges |> as.data.frame()

    # Create Skeleton Graph
    skeleton <- graph_from_data_frame(
        d = edges_df,
        vertices = nodes_df,
        directed = FALSE
    )

    # --- ADD OTHER ATTRIBUTES ---

    # Edge Strength
    if (!is.null(ballmapper_output$edges_strength)) {
        E(skeleton)$weight <- ballmapper_output$edges_strength
    }

    # Node Coverage
    if (!is.null(ballmapper_output$points_covered_by_landmarks)) {
        V(skeleton)$members <- ballmapper_output$points_covered_by_landmarks
    }

    # Coloring Values
    if (!is.null(ballmapper_output$coloring)) {
        V(skeleton)$coloring <- ballmapper_output$coloring
    }

    # Landmarks
    if (!is.null(ballmapper_output$landmarks)) {
        V(skeleton)$landmarks <- ballmapper_output$landmarks
    }

    # --- Return igraph object ---
    return(skeleton)

}