#' @Author: Luísa Viegas, Leonardo Brait
#' @description
#' @requirements R 4.3.1 Ubuntu 20.04


################################# Environment ##################################
library("tidyverse") #2.0.0
library("sf") #1.0.14
library("ggplot2") #3.4.2
library("raster") #3.6.23
library("corrr") #0.4.4
library("psych") #2.3.9
library("CoordinateCleaner") #3.0.1
library("rnaturalearth") #0.3.4

############################## Ceara Delimitation ##############################
brasil <- rnaturalearth::ne_countries(country = "Brazil", returnclass = "sf")
ceara <- st_read("data/shapes/CE_UF_2022.shp")
conservation_units <- st_read("data/shapes/uc_ceara.shp")

############################## Raster Processing ###############################
tif_files_paths <- dir("data/raster_raw", pattern = ".tif", full.names = TRUE)
rasters <- raster::stack(tif_files_paths)
climate_ceara <- raster::crop(x = rasters, y = ceara) %>%
  raster::mask(mask = ceara)

############################## Correlation Analysis ############################
abiotics_table <- climate_ceara %>%
  raster::values() %>%
  tibble::as_tibble() %>%
  tidyr::drop_na()
correlation_table <- abiotics_table %>%
  corrr::correlate(method = "spearman")
high_correlated_vars <- correlation_table %>%
  corrr::as_matrix() %>%
  caret::findCorrelation(cutoff = 0.8, names = TRUE, verbose = TRUE)
abioticas_tabeladas_filtradas <- abiotics_table %>%
  dplyr::select(-high_correlated_vars)

# data visualization -------------------
psych::pairs.panels(
  x = abioticas_tabeladas_filtradas %>% 
    dplyr::sample_n(253),
  method = "spearman",
  pch = 20,
  ellipses = FALSE,
  density = FALSE,
  stars = TRUE,
  hist.col = "gray",
  digits = 2,
  rug = FALSE,
  breaks = 10,
  ci = TRUE
)


############################## Saving Rasters ##################################
if (!dir.exists("data/raster_processed")) {
  dir.create("data/raster_processed")
}

climate_ceara <- climate_ceara[[abioticas_tabeladas_filtradas %>% names()]]
for (i in 1:nlayers(climate_ceara)) {
  raster::writeRaster(
    x = climate_ceara[[i]],
    filename = paste0(
      "data/raster_processed/climate_ceara_", names(climate_ceara)[i]
    ),
    bylayer = TRUE,
    options = c("COMPRESS=DEFLATE"),
    format = "GTiff",
    overwrite = TRUE
  )
}

########################## Occurrencies Filtering ##############################
occurencies <- read_csv("data/occurrencies/soldadinho.csv") %>%
  tidyr::drop_na(lon, lat)

flags_spatial <- CoordinateCleaner::clean_coordinates(
  x = occurencies,
  species = "species",
  lon = "lon",
  lat = "lat",
  tests = c(
    "capitals", # 10km raio ao redor de capitais
    "centroids", # 1km raio ao redor de centroides de paises e provincias
    "duplicates", # duplicatas
    "equal", # coordenadas iguais
    "gbif", # raio ao redor da sede da GBIF
    "institutions", # raio de instituicoes de pesquisa em biodiversidade
    "seas", # pontos no mar
    "validity", # ponto de fora do sistema de coordenadas
    "zeros" # zeros e pontos onde lat = lon
  )
)

clean_occurencies <- occurencies %>%
    dplyr::filter(flags_spatial$.summary == TRUE)
write_csv(clean_occurencies, "data/occurrencies/soldadinho_clean.csv")

############################## Data Visualization ##############################
if (!dir.exists("results")) {
  dir.create("results")
}
ggplot() +
  geom_sf(data = brasil, fill = "darkgreen", color = "black") +
  geom_sf(data = ceara, fill = "white", color = "black") +
  geom_sf(data = conservation_units, fill = "blue", alpha = 1) +
  geom_point(data = occurencies, aes(x = lon, y = lat), color = "red") +
  labs(x = NULL, y = NULL) +
  theme_bw()
ggsave("results/mapa_brasil.svg", width = 10, height = 10, dpi = 300)
ggsave("results/mapa_brasil.png", width = 10, height = 10, dpi = 300)

