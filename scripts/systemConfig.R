###########################
## Load System Cofiguration
###########################

# Clear Memory
rm(list = ls())

# Load Packages -----------------------------------------------------------
if(!require("pacman")){
    install.packages("pacman")
}

# Will install following packages if not already installed
pacman::p_load(
    tidyverse,
    BallMapper,
    viridis,
    knitr,
    kableExtra,
    bea.R,
    svglite
)
