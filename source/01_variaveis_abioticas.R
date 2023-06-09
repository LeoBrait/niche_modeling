# Drawing a map of Brazil
poligono_geografico <- readOGR(
  "dados/shapes/sul_ceara/Poligono_ceara_circulo.shp")

# load world rasters names and stack in a single object
tif_files_paths <- dir("dados/raster", pattern = ".tif", full.names = TRUE)
rasters <- raster::stack(tif_files_paths)

# cut the stacked rasters based on the Brazil shapefile
raster_brasil <- raster::crop(x = rasters, y = poligono_geografico) %>%
  raster::mask(mask = poligono_geografico)

# informações sobre a resolução do raster
raster::res(raster_brasil)[1] # grau decimal
raster::res(raster_brasil)[1] / (30 / 3600) # km x km

# # fator de agregacao
# resolucao_integral <- raster::res(raster_brasil)[1]
# resolucao_desejada <- 1

# #fator de agregacao:
# fator_agregante <- resolucao_desejada / resolucao_integral

# # agregacao
# raster_brasil_baixaresolucao <- raster::aggregate(
#   raster_brasil, fact = fator_agregante)

raster_brasil_baixaresolucao <- raster_brasil

# nova resolucao
raster::res(raster_brasil_baixaresolucao)[1]
raster::res(raster_brasil_baixaresolucao)[1] / (30 / 3600)


# salva o raster cortado e na nova resolu??o numa nova pasta
for (i in 1:nlayers(raster_brasil_baixaresolucao)) {
  raster::writeRaster(
    x = raster_brasil_baixaresolucao[[i]],
    filename = paste0(
              "dados//raster_reduced//raster_br_res05_",
               names(raster_brasil)[i]),
    bylayer = TRUE,
    options = c("COMPRESS=DEFLATE"),
    format = "GTiff",
    overwrite = TRUE)
}

# Avaliar a correla??o entre as vari?veis clim?ticas
# cria uma nova pasta para a an?lise de correlacao
# extrai os valores
abioticas_tabeladas <- raster_brasil_baixaresolucao %>%
  raster::values() %>%
  tibble::as_tibble() %>%
  tidyr::drop_na()

# correlacao - utilizaremos aqui o metodo de Spearman
cor_table <- corrr::correlate(abioticas_tabeladas, method = "spearman")

# cria uma tabela com o resultado da correlacao
sumario_correlacao <- cor_table %>%
  corrr::shave() %>%
  corrr::fashion()

# e exporta a tabela
readr::write_csv(
  sumario_correlacao,
  "results/correlacao_variaveisambientais.csv")


# selecao das variaveis correlacionadas
variaveis_para_exclusao <- cor_table %>%
  corrr::as_matrix() %>%
  caret::findCorrelation(cutoff = 0.8, names = TRUE, verbose = TRUE)
variaveis_para_exclusao

abioticas_tabeladas_filtradas <- abioticas_tabeladas %>%
  dplyr::select(-variaveis_para_exclusao)
abioticas_tabeladas_filtradas

pairs.panels(x = abioticas_tabeladas_filtradas %>% dplyr::sample_n(253),
             method = "spearman",
             pch = 20,
             ellipses = FALSE,
             density = FALSE,
             stars = TRUE,
             hist.col = "gray",
             digits = 2,
             rug = FALSE,
             breaks = 10,
             ci = TRUE)
dev.off()

# a partir da analise feita, seleciona as variaveis que nao se correlacionam 
# para prosseguir com a modelagem
# fim da parte 1 :)