source("source/utilities/install_and_load.R")
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
    "wallace",
    "rgdal"
    ))

#Non-CRAN packages
if (!require(wesanderson)) {
    devtools::install_github("karthik/wesanderson")} else {
        library(wesanderson)}

source("source/01_variaveis_abioticas.R")
source("source/02_filtragem_dos_dados.R")
source("source/03_algoritmos.R")
source()
