# -------------------------------------------------------------------------
### Modelagem de nicho ecologico: Teoria e pratica 
# Binarizacao e limite de corte

# Prof. Luisa Maria Diele-Viegas e Juliana Hipolito
# -------------------------------------------------------------------------


occ <- readr::read_csv("input/ocorrencias_bioticas_abioticas.csv")


for(i in occ$species %>% unique){
  
  # buscando os dados de consenso
  # informacao 
  print(paste0("Binarizate weighted average to ", i))
  
  
  # importando os dados gerados
  ens_w <- dir("results/predictions", pattern = ".tif", full.names = TRUE) %>% 
    raster::stack()
  
  # extraindo valores considerando a distribuicao 
  thrs <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(lon, lat) %>% 
    raster::extract(ens_w, .)
  
  # limites de corte - iremos considerar um limite m?nimo >0, de 30% e de 50%
  li_thrs <- list(
    lpt = min(thrs[thrs > 0]),
    p10 = quantile(thrs[thrs > 0], .3),
    p20 = quantile(thrs[thrs > 0], .5)
  )

  
  for(j in li_thrs %>% length %>% seq){
    
    # exporta os dados obtidos 
    raster::writeRaster(x = ens_w >= li_thrs[[j]], 
                        filename = paste0("consenso_thr_", names(li_thrs)[j], "_", i), 
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        overwrite = TRUE)
    
  }

}

getwd()
# Estamos quase la! :)