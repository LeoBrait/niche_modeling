source("utilities/install_and_load.R")
install_and_load(libs = c(
    "tidyverse",
    "lubridate",
    "spocc",
    "taxize",
    "CoordinateCleaner",
    "sf",
    "ggspatial",
    "landscapetools",
    "raster",
    "rgdal",
    "devtools",
    "rnaturalearth",
    "corrr",
    "caret",
    "psych",
    "dismo",
    "kernlab",
    "randomForest",
    "vegan",
    "beepr",
    "spThin",
    "rgeos",
    "ENMeval",
    "wallace"
    ))

## check package version
packageVersion("wallace")


#Non-CRAN packages
if (!require(wesanderson)) {
    devtools::install_github("karthik/wesanderson")} else {
        library(wesanderson)}

occurences <- read_csv("input/microgroups_prevalence_persite.csv")
occurences <- occurences %>%
filter(meanAbu > 0)

cpr_occurences <- occurences %>%
filter(microGroup == "CPR") %>%
dplyr::select(
    microGroup,
    latitude,
    longitude)

dpann_occurences <- occurences %>%
filter(microGroup == "DPANN") %>%
dplyr::select(
    microGroup,
    latitude,
    longitude)

dummy <- read_csv("input/fulanus.csv")
