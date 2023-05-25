# aqui coloque o caminho para a pasta que contem o scrips main.R
setwd()

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
source("source/04_resumo_avaliacoes.R")
source("source/05_consenso.R")
source("source/06_limite_de_corte.R")
source("source/07_mapas.R")
