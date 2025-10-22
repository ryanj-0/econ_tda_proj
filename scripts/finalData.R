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


lastPulled <- Sys.Date()



# Quarterly Combined ------------------------------------------------------

BEA_quarterly <- inner_join(GDP_Q, PID_Q, by = join_by(year, quarter))

FRED_quarterly <-map(FRED_data, "quarterly")

