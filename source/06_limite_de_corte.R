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
    p10 = quantile(thrs[thrs > 0], .1),
    p30 = quantile(thrs[thrs > 0], .3),
    p60 = quantile(thrs[thrs > 0], .6),
    p90 = quantile(thrs[thrs > 0], .9)
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

# plot(ens_w >= li_thrs[[2]], main = "Limite de corte 10%")
# plot(ens_w >= li_thrs[[3]], main = "Limite de corte 30%")
# plot(ens_w >= li_thrs[[4]], main = "Limite de corte 60%")
# plot(ens_w >= li_thrs[[5]], main = "Limite de corte 90%")




# Estamos quase la! :)