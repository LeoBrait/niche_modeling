#' @Author: Luísa Viegas, Leonardo Brait
#' @description
#' @requirements R 4.3.1 Ubuntu 20.04

################################# Environment ##################################
library("tidyverse") #2.0.0
library("raster") #3.6.23
threshold <- .8

################################### Ensemble ###################################
if (!dir.exists("data/ensembled")) {
  dir.create("data/ensembled")
}

scores <- read_csv("data/predictions/scores.csv")
scores_filtered <- scores %>% 
  dplyr::filter(tss_spec_sens >= threshold)
rasters <- scores_filtered %>%
    dplyr::pull(file) %>%
    raster::stack()
rasters_standarized <- rasters %>%
  raster::values() %>%
  vegan::decostand("range", na.rm = TRUE)
ensembled <- rasters[[1]]
ensembled[] <- apply(
  rasters_standarized, 1, 
  function(x){
    sum(x * scores_filtered$tss_spec_sens) / sum(scores_filtered$tss_spec_sens)
  }
)
raster::writeRaster(
  x = ensembled, 
  filename = "data/ensembled/ensembled_soldadinho", 
  format = "GTiff", 
  options = c("COMPRESS=DEFLATE"), 
  overwrite = TRUE
)
################################## Observation #################################
# removed the following chunk from original code:
#tss_spec_sens <- scores_filtered %>%
# dplyr::select(tss_spec_sens) %>%
# dplyr::mutate(tss_spec_sens = (tss_spec_sens - .5) ^ 2) %>%
# dplyr::pull()
# These values were used to weight the rasters in the ensemble, but i do not
# know why original authors used this transformation.
  
