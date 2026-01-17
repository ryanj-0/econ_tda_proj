#####################################
## Data for Ball Mapper Analysis
#####################################

# General Selection of Analysis Data --------------------------------------
# From our Sources section, we select:
dataSources <- c("PPI_Finished",
                 "PID",
                 "GDP",
                 "CPI",
                 "unemployment",
                 "FFR",
                 "housingStarts")


analysisData <- all_annual[dataSources]

# Mutate cols in `change_tbl` to calculate pct annual change using log diff.
change_tbl <- c("PPI_Finished", "CPI", "housingStarts")
analysisData <- map2(analysisData, names(analysisData),
                     ~ {
                         if(.y %in% change_tbl) {
                             seriesID <- unique(.x$series_id)

                             .x |>
                                 mutate(pctChange =
                                            (log(avgA) - log(lag(avgA)))*100) |>
                                 rename_with(~ paste0(., "_", seriesID),
                                             c(avgA, pctChange)
                                 )
                             } else {
                                 .x
                             }
                         }
)


# Changing Specific Table Cols --------------------------------------------

analysisData[["PPI_Finished"]] <- analysisData[["PPI_Finished"]] |>
    select(year, pctChange_WPSFD49207)

# Reduce Personal Income and it's Disposition columns.
# Since this is a large macroeconomic analysis we are going to try and
# keep the large categories
# - Personal Income
# - Employee Compensation
# - Proprietors' Income w/ Invtry. Val. ... (Small Businesses)
# -

analysisData[["PID"]] <- analysisData[["PID"]] |>
    select(
        year,
        Personal_income_PID,
        Compensation_of_employees_PID,
        `Proprietors'_income_with_inventory_valuation_and_capital_consumption_adjustments_PID`,
        Personal_current_transfer_receipts_PID
        )

# Reducing GDP cols for computational and analysis purposes
# Removing Current dollars col
analysisData[["GDP"]] <- analysisData[["GDP"]] |>
    select(
        year,
        Gross_domestic_product_GDP,
        Personal_consumption_expenditures_GDP,
        Gross_private_domestic_investment_GDP,
        Government_consumption_expenditures_and_gross_investment_GDP,
        Exports_GDP,
        Imports_GDP
        )

analysisData[["CPI"]] <- analysisData[["CPI"]] |>
    select(year, pctChange_CPIAUCSL)

analysisData[["unemployment"]] <- analysisData[["unemployment"]] |>
    rename("unrate" = avgA) |>
    mutate(pctChange_UNRATE = unrate - lag(unrate)) |>
    select(year, unrate, pctChange_UNRATE)

analysisData[["FFR"]] <- analysisData[["FFR"]] |>
    mutate(pctChange_FFR = rate_FFR - lag(rate_FFR)) |>
    select(year, rate_FFR, pctChange_FFR)

analysisData[["housingStarts"]] <- analysisData[["housingStarts"]] |>
    select(year, pctChange_HOUST)

# Reduce tables to select years
analysisYears <- c(1960:2024)
analysisData <- map(analysisData, ~ filter(.x, year %in% analysisYears))

# Reduce and combine tables
final_data <- reduce(analysisData, full_join, by = "year")


# Final Data to Reference -------------------------------------------------

final_data <- final_data |>
    left_join(nberRecessions |> select(year, recession_span)) |>
    left_join(nberExpansion |> select(year, expansion_span)) |>
    rename(Year = year,
           PPI_Finished_Change = pctChange_WPSFD49207,
           Personal_Income = Personal_income_PID,
           Compensation = Compensation_of_employees_PID,
           Entrepreneurship =
               `Proprietors'_income_with_inventory_valuation_and_capital_consumption_adjustments_PID`,
           Transfers = Personal_current_transfer_receipts_PID,
           GDP = Gross_domestic_product_GDP,
           Consumption = Personal_consumption_expenditures_GDP,
           Domestic_Investment = Gross_private_domestic_investment_GDP,
           Government_Spending =
               Government_consumption_expenditures_and_gross_investment_GDP,
           Exports = Exports_GDP,
           Imports = Imports_GDP,
           Inflation = pctChange_CPIAUCSL,
           Unemployment = unrate,
           Unemployment_Change = pctChange_UNRATE,
           Fed_Rate = rate_FFR,
           Fed_Rate_Change = pctChange_FFR,
           Housing_Change = pctChange_HOUST
    ) |>
    mutate(
        row_id = row_number(),
        # Macro Analysis Variables
        Fisher_Equation = Fed_Rate - Inflation,
        Margin_Gap = Inflation - PPI_Finished_Change,
        Non_Income_Labor_Gap = Personal_Income - Compensation,
        Ex_Transfers = Personal_Income - Transfers)

pointcloud <- final_data |>
    select(-c(
        Year,
        row_id,
        GDP,
        recession_span,
        expansion_span,
        Fisher_Equation,
        Margin_Gap,
        Non_Income_Labor_Gap,
        Ex_Transfers
    )) |>
    as.data.frame() |>
    normalize_to_min_0_max_1()

# clean up variable enviornment
rm(all_quarterly,
   nber_business_cycles,
   nberExpansion,
   nberRecessions)
