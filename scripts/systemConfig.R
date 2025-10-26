############################
## Load System Configuration
############################

# Clear Memory
rm(list = ls())

# Load Packages -----------------------------------------------------------
if (!require("pacman")) {
    install.packages("pacman")
}

# Will install following packages if not already installed
pacman::p_load(tidyverse,
               bea.R,
               fredr,
               BallMapper,
               viridis,
               knitr,
               kableExtra,
               svglite,
               showtext)


# Source Functions --------------------------------------------------------

fxnList <- list.files(paste(getwd(), "functions", sep = "/"), full.names = TRUE)

lapply(fxnList, source) |> invisible()

# API Keys ------------------------------------------------------------

# Load API Keys
source(paste(getwd(), "scripts/my_apiKeys.R", sep = "/"))

# Set FRED API Key
fredr_set_key(my_fredKey)


# Font Configuration ------------------------------------------------------
font_add_google("EB Garamond")
showtext_auto()
