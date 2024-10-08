#' @Author: Luísa Viegas, Leonardo Brait
#' @description Load the ensembled raster and binarize it using different 
#' thresholds.
#' @requirements R 4.3.1 Ubuntu 20.04

################################# Environment ##################################
library("tidyverse") #2.0.0
library("raster") #3.6.23
library("ggspatial") #1.1.9
library("sf") #1.0.14
library("ggpubr") #3.3.5

thresholds <- c(
  "10p" = 0.1,
  "20p" = 0.2,
  "30p" = 0.3,
  "40p" = 0.4,
  "50p" = 0.5,
  "60p" = 0.6,
  "70p" = 0.7,
  "80p" = 0.8,
  "90p" = 0.9
)

if (dir.exists("results") == FALSE) {
  dir.create("results")
}
################################# Load Data ####################################
occurrencies <- read_csv("data/occurrencies/soldadinho_clean.csv")
ensembled_raster <- 
  dir("data/ensembled/", pattern = ".tif", full.names = TRUE) %>%
  raster::stack()
conservation_units <- st_read("data/shapes/uc_ceara.shp")

################################# Binarization #################################
for(i in 1:length(thresholds)){

  raster::writeRaster(
    x = ensembled_raster >= thresholds[i],
    filename = paste0("results/binarization_", names(thresholds)[i]),
    format = "GTiff",
    options = c("COMPRESS=DEFLATE"), 
    overwrite = TRUE
  )
  
  output_file <- paste0("results/", names(thresholds)[i], ".png")
  png(output_file)
  plot(
    ensembled_raster >= thresholds[i], 
    main = paste("Limite de corte", sub("p", " %", names(thresholds)[i])),
    cex.main = 2
  )
  dev.off()
}
################################# Final Plot ###################################
# Crie um objeto SF do Brasil
brasil_sf <- st_read("data/shapes/BR_UF_2022.shp")

# Crie o plot do mapa do Ceará
map <- ggplot() +
  geom_raster(
    data = raster::rasterToPoints(ensembled_raster) %>% 
      tibble::as_tibble(),
    aes(x, y, fill = log(ensembled_soldadinho))
  ) +
  geom_sf(data = conservation_units, fill = NA, color = "black", size = 5) +
  geom_point(
    data = occurrencies, aes(lon, lat), size = 2, alpha = .7, shape = 3, color = "red"
  ) +
  theme_pubr() +
  scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
  scale_fill_gradientn(
    colours = c("white", "green", "blue"), 
    values = c(0, 1)
  ) +
  labs(
    x = "Longitude", y = "Latitude", fill = "Adequability (log)", color = "Occurrence"
  ) +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  theme(
    title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.background = element_rect(
      fill = "white", size = 0.3, linetype = "solid", colour = "black"
    ),
    axis.title = element_text(size = 15, face = "plain"),
  )

# Crie o mini-mapa do Brasil com destaque no Ceará
mini_map <- ggplot() +
  geom_sf(data = brasil_sf, fill = "gray80", color = "black") +
  geom_sf(data = brasil_sf %>% filter(NM_UF == "Ceará"), fill = "green", color = "black") + # Destaque para o Ceará
  theme_void() # Remove todos os elementos do tema

# Adicione o mini-mapa ao mapa do Ceará
map <- map +
  annotation_custom(
    ggplotGrob(mini_map),
    xmin =-43 , xmax =-41.8 , ymin = -9.4, ymax = -6.6
  )

# good xmin = -41.6, xmax = -40.5, ymin = -8.3, ymax = -6.7
#x 1.1 y 1.6
ggsave(
  "results/adequabilidade_final.png", 
  map, wi = 20, he = 20, un = "cm", dpi = 300
) 
