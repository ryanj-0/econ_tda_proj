######################################################
## Robustness Section for paper, Methods section
######################################################

random_check <- function() {

    # shuffle final_data
    shuffled_data <- final_data |>
        slice_sample(n = nrow(final_data))

    # set point cloud for new shuffled data frame
    pointcloud <- shuffled_data |>
        select(-c(year, row_id, recession_span, expansion_span)) |>
        as.data.frame() |>
        normalize_to_min_0_max_1()

    # set year for coloring to keep consistency
    coloring <- shuffled_data |>
        select(year) |>
        as.data.frame()

    # shuffled BallMapper
    shuffled_bm <- BallMapper(points = pointcloud,
                     values = coloring,
                     epsilon = 0.511)

}