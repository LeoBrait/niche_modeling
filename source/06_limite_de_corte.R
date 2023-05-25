# -------------------------------------------------------------------------
### Modelagem de nicho ecologico: Teoria e pratica 
# Binarizacao e limite de corte

# Prof. Luisa Maria Diele-Viegas e Juliana Hipolito
# -------------------------------------------------------------------------


occ <- tabela_referencia


for(i in occ$species %>% unique){
  
  # buscando os dados de consenso
  # informacao 
  print(paste0("Binarizate weighted average to ", i))
  
  
  # importando os dados gerados
  ens_w <- dir("results/ensemble/", pattern = ".tif", full.names = TRUE) %>%
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
                        filename = paste0("results/nichos_modelados/consenso_cortado", names(li_thrs)[j], "_", i),  #nolint
                        format = "GTiff", 
                        options = c("COMPRESS=DEFLATE"), 
                        overwrite = TRUE)

  }

}

plot(ens_w >= li_thrs[[1]])
plot(ens_w >= li_thrs[[2]])
plot(ens_w >= li_thrs[[3]])

getwd()
# Estamos quase la! :)