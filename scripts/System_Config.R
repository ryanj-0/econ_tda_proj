############################
## Load System Configuration
############################

# Clear Memory
rm(list = ls())
gc()


# Load Packages -----------------------------------------------------------
if (!require("pacman")) {
    install.packages("pacman")
}

library(pacman)

pkgs <- c(
    # APIs
    "bea.R",
    "fredr",
    # Paper formatting and design
    "gt",
    "svglite",
    "showtext",
    "knitr",
    "patchwork",
    # Analysis
    "future",
    "furrr",
    "corrr",
    "microbenchmark",
    "ggraph",
    "igraph",
    "tidygraph",
    "BallMapper",
    "tidyverse"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))
pacman::p_load(char = pkgs)

# Source Functions --------------------------------------------------------

fxnList <- list.files(paste(getwd(), "functions", sep = "/"), full.names = TRUE)

lapply(fxnList, source) |> invisible()

# API Keys ------------------------------------------------------------

# Load API Keys
source(paste(getwd(), "scripts/my_apiKeys.R", sep = "/"))

# Set FRED API Key
fredr_set_key(my_fredKey)


# Font Configuration ------------------------------------------------------
sysfonts::font_add_google("EB Garamond")
showtext::showtext_auto()
