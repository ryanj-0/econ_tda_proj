############################
## Load System Configuration
############################

# Clear Memory
rm(list = ls())

# Load Packages -----------------------------------------------------------
if(!require("pacman")){
    install.packages("pacman")
}

# Will install following packages if not already installed
pacman::p_load(
    tidyverse,
    bea.R,
    fredr,
    BallMapper,
    viridis,
    knitr,
    kableExtra,
    svglite
)


# Source Functions --------------------------------------------------------

fxnList <-
    list.files(paste(getwd(), "functions", sep = "/"),
               full.names = TRUE)

lapply(fxnList, source) |> invisible()

# API Keys ------------------------------------------------------------

# Load API Keys
source(paste(getwd(), "my_apiKeys.R", sep = "/"))

# Set FRED API Key
fredr_set_key(my_fredKey)

