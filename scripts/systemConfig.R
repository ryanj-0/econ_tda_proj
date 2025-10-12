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


# API Keys ------------------------------------------------------------

# Load API Keys
source("my_apiKeys.R")

# Set FRED API Key
fredr_set_key(my_fredKey)
