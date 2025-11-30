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

# Will install following packages if not already installed
pkgs <- c(
    # APIs
    "bea.R",
    "fredr",
    # Paper formatting and design
    "gt",
    "viridis",
    "kableExtra",
    "svglite",
    "showtext",
    "knitr",
    # Analysis
    "future",
    "furrr",
    "microbenchmark",
    "igraph",
    "tidygraph",
    "ggraph",
    "BallMapper",
    "data.table",
    "tidyverse"
)

# Package Load Function
pkg_loader <- function(pkg) {
    # Check if installed
    if (!requireNamespace(pkg, quietly = TRUE)) {
        message(paste("Installing missing package:", pkg))
        install.packages(pkg)
    }

    # Try to load it. If this fails, the script STOPS here.
    tryCatch({
        library(pkg, character.only = TRUE)
        message(paste("✔ Loaded:", pkg))
    }, error = function(e) {
        stop(paste("❌ FAILED to load:", pkg, "\nError message:", e$message))
    })
}

lapply(pkgs, pkg_loader)




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
