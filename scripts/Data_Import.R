############################################
## Importing Raw Data from APIs.
## Data wrangling included in scripts.
## Run finalData.R after this script
############################################


# Check Data Pull Date ----------------------------------------------------

lastPulled <- readRDS(paste(getwd(), "data/lastPulled.rds", sep = "/"))

if(Sys.Date() > lastPulled + 10) {

    # Update last pulled
    lastPulled <- Sys.Date()
    saveRDS(lastPulled, paste(getwd(), "data/lastPulled.rds", sep = "/"))

    # Pull data again ---------------------------------------------------------
    # Import FRED Data
    # Data Sources:
    # - CPI
    # - Housing Starts
    # - PPI: All Commodities
    # - PPI: Final Demand
    # - Unemployment
    # - Recession Dates
    source(paste(getwd(), "scripts/importFRED.R", sep = "/"))

    # Import BEA NIPA Data: data includes Annual and Quarterly
    # - GDP
    # - Personal Income and Its Disposition
    source(paste(getwd(), "scripts/importBEA_NIPA.R", sep = "/"))

    # Import BEA Foreign Direct Investment
    source(paste(getwd(), "scripts/importBEA_FDI.R", sep = "/"))

    # Clean up
    rm(nipaConfig,
       gdpA_list, gdpQ_list,
       pidA_list, pidQ_list,
       FFR_M, seriesTable)

} else
    message("Last Date Pulled: ", lastPulled, "\n",
            "Data within 10 days of last pull, loading local data.")




