############################################
## Importing Raw Data from APIs.
## Data wrangling inlcuded in scripts.
############################################


# Check Data Pull Date ----------------------------------------------------

lastPulled <- readRDS(paste(getwd(), "data/lastPulled.rds", sep = "/"))

if(Sys.Date() > lastPulled + 90) {

    # Update last pulled
    lastPulled <- Sys.Date()
    saveRDS(lastPulled, paste(getwd(), "data/lastPulled.rds", sep = "/"))

    # Pull data again ---------------------------------------------------------
    # Import BEA NIPA Data: data includes Annual and Quartely
    # - GDP
    # - Personal Income and Its Disposition
    source(paste(getwd(), "scripts/importBEA_NIPA.R", sep = "/"))

    # Clean up
    rm(nipaConfig,
       gdpA_list, gdpQ_list,
       pidA_list, pidQ_list)


    # Import BEA Foreign Direct Investment
    source(paste(getwd(), "scripts/importBEA_FDI.R", sep = "/"))


    # Import FRED Data
    # Data Sources:
    # - CPI
    # - Housing Starts
    # - PPI: All Commodities
    # - PPI: Final Demand
    # - Unemployment
    # - Recession Dates
    source(paste(getwd(), "scripts/importFRED.R", sep = "/"))

    # clean up
    rm(FFR_M, seriesTable)

} else
    message("Data within 90 days of last pull, loading local data.")




