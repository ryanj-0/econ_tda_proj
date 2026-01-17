############################################
## Importing Raw Data from APIs.
## Data wrangling included in scripts.
## Run finalData.R after this script
############################################


# Check Data Pull Date ----------------------------------------------------

last_pulled <- readRDS(paste(getwd(), "data/Last_Pulled.rds", sep = "/"))

if(Sys.Date() > last_pulled + 3) {

    # Update last pulled
    last_pulled <- Sys.Date()
    saveRDS(last_pulled, paste(getwd(), "data/Last_Pulled.rds", sep = "/"))

    # Pull data again ---------------------------------------------------------
    # Import FRED Data
    # Data Sources:
    # - CPI
    # - Housing Starts
    # - PPI: All Commodities
    # - PPI: Final Demand
    # - Unemployment
    # - Recession Dates
    source(paste(getwd(), "scripts/Data_import_FRED.R", sep = "/"))

    # Import BEA NIPA Data: data includes Annual and Quarterly
    # - GDP
    # - Personal Income and Its Disposition
    source(paste(getwd(), "scripts/Data_import_BEA_NIPA.R", sep = "/"))

    # Import BEA Foreign Direct Investment
    source(paste(getwd(), "scripts/Data_import_BEA_FDI.R", sep = "/"))

    # Clean up
    rm(nipaConfig,
       gdpA_list, gdpQ_list,
       pidA_list, pidQ_list,
       seriesTable)

} else
    message("Last Date Pulled: ", last_pulled, "\n",
            "Data within 3 days of last pull, loading local data.")




