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

if(Sys.Date() > lastPulled + 90) {

    # Quarterly Combined

    BEA_quarterly <- inner_join(GDP_Q, PID_Q, by = join_by(year, quarter))

    FRED_quarterly <-map(FRED_data, "quarterly")
    # Dropping "PPI_Final" b/c short year range
    FRED_quarterly[["PPI_Final"]] <- NULL

    all_quarterly <- c(list(GDP = GDP_Q,
                            PID = PID_Q,
                            FFR = FFR_Q,
                            ECI = ECI_Q
    ),
    FRED_quarterly
    )


    # Annual Combined

    # Not including FDI_A data because short year range
    BEA_annual <- inner_join(GDP_A, PID_A, by = join_by(year))

    FRED_annual <-map(FRED_data, "annual")
    # Dropping "PPI_Final" b/c short year range
    FRED_annual[["PPI_Final"]] <- NULL

    all_annual <- c(list(GDP = GDP_A,
                         PID = PID_A,
                         FDI = FDI_A,
                         FFR = FFR_A,
                         ECI = ECI_A
    ),
    FRED_annual
    )

    # Save Updated Data
    saveRDS(all_quarterly, paste(getwd(), "data/all_quarterly.rds", sep = "/"))
    saveRDS(all_annual, paste(getwd(), "data/all_annual.rds", sep = "/"))

} else {

    all_quarterly <- readRDS(paste(
        getwd(),
        "data/all_quarterly.rds",
        sep = "/")
    )

    all_annual <- readRDS(paste(getwd(), "data/all_annual.rds", sep = "/"))
}

