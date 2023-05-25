# -------------------------------------------------------------------------
### Modelagem de nicho ecologico: Teoria e pratica 
# Consenso por media ponderada

# Prof. Luisa Maria Diele-Viegas e Juliana Hipolito
# -------------------------------------------------------------------------
# listando os arquivos das avaliacoes


# importando os diferentes modelos
eva <- read_csv("results/predictions/eval_fulanus_fulanus.csv")
eva

# consenso por media ponderada
# Utilizaremos como limite inferior AUCs com valores de 0.8

auc_limit <- .8

# algoritmos
alg <- eva$algorithm %>% unique
alg
# fazendo o consenso
for(i in eva$species %>% unique){

  # selecao de modelos = somente aqueles com AUC maior ou igual a 0.8
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  auc >= auc_limit, 
                  algorithm %in% alg)
  
  # importando os modelos
  enm <- eva_i %>% 
    dplyr::select(file) %>% 
    dplyr::pull() %>% 
    raster::stack()
  
  # AUC
  auc <- eva_i %>% 
    dplyr::select(auc) %>% 
    dplyr::mutate(auc = (auc - .5) ^ 2) %>% 
    dplyr::pull()
  
  # padronizacao 
  print("Pode demorar... mas vai dar bom!")
  enm_st <- enm %>% 
    values %>% 
    vegan::decostand("range", na.rm = TRUE)
  print("N?o disse? Sucesso!")

  # consenso da media ponderada 
  ens <- enm[[1]]
  ens[] <- apply(enm_st, 1, function(x){sum(x * auc) / sum(auc)})
  
  # diretorio de trabalho 
  
  # exporta o ensemble 
  raster::writeRaster(x = ens, 
                      filename = paste0("presente_", i), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      overwrite = TRUE)
  
} 

# Fimmmm! :)
