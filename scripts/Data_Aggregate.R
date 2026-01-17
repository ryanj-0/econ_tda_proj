########################################################
## Combine all data sources and return 2 tables:
## - Quarterly
## - Annual
## Note:
##     Quarterly data does not include FDI, only
## available in annual.
##     We aggregated a table of recession data to
## to include in some BallMapper analysis. We pulled
## our recessions data from FRED.
#######################################################


# Update Final Data Check --------------------------------------------------

if(Sys.Date() > last_pulled + 3) {

    # Quarterly Combined
    BEA_quarterly <- inner_join(GDP_Q, PID_Q, by = join_by(year, quarter))
    FRED_quarterly <-map(FRED_data, "quarterly")

    # Quarterly Combined
    all_quarterly <- c(
        list(GDP = GDP_Q,
             PID = PID_Q,
             ECI = ECI_Q
        ),
        FRED_quarterly
    )


    # Annual Combined
    # Not including FDI_A data because short year range
    BEA_annual <- inner_join(GDP_A, PID_A, by = join_by(year))

    FRED_annual <-map(FRED_data, "annual")

    all_annual <- c(
        list(GDP = GDP_A,
             PID = PID_A,
             FDI = FDI_A,
             FFR = FFR_A,
             ECI = ECI_A
        ),
        FRED_annual
    )

    # Save Updated Data
    saveRDS(all_quarterly, paste(getwd(), "data/All_Quarterly.rds", sep = "/"))
    saveRDS(all_annual, paste(getwd(), "data/All_Annual.rds", sep = "/"))

} else {

    all_quarterly <- readRDS(
        paste(getwd(), "data/All_Quarterly.rds", sep = "/")
        )

    all_annual <- readRDS(paste(getwd(), "data/All_Annual.rds", sep = "/"))

}

# Recession Dates ---------------------------------------------------------

nber_business_cycles <-
    fredr(series_id = "USRECQ") |>
    mutate(
        year = format(date, "%Y") |> as.numeric(),
        month = format(date, "%m") |> as.numeric(),
        quarter = case_when(
            month %in% c(1:3) ~ 1,
            month %in% c(4:6) ~ 2,
            month %in% c(7:9) ~ 3,
            month %in% c(10:12) ~ 4
        )
    )

nberRecessions <- nber_business_cycles |>
    filter(value == 1) |>
    group_by(year) |>
    mutate(recession_span = n()/4) |>
    select(series_id, year, recession_span) |>
    unique()

nberExpansion <- nber_business_cycles |>
    filter(value == 0) |>
    group_by(year) |>
    mutate(expansion_span = n()/4) |>
    select(series_id, year, expansion_span) |>
    unique()
