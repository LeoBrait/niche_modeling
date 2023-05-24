
# Drawing a map of Brazil
br <- rnaturalearth::ne_countries(country = "Brazil", returnclass = "sf")

# load world rasters names and stack in a single object
tif_files_paths <- dir("input/raster", pattern = ".tif", full.names = TRUE)
rasters <- raster::stack(tif_files_paths)

# cut the stacked rasters based on the Brazil shapefile
raster_brasil <- raster::crop(x = rasters, y = br) %>%
  raster::mask(mask = br)

# informações sobre a resolução do raster
raster::res(raster_brasil)[1] # grau decimal
raster::res(raster_brasil)[1] / (30 / 3600) # km x km

# fator de agregacao
#resolucao atual:
resolucao_integral <- raster::res(raster_brasil)[1]

#resolucao que a gente quer:
resolucao_desejada <- 0.5

#fator de agregacao:
fator_agregante <- resolucao_desejada / resolucao_integral

# agregacao
raster_brasil_baixaresolucao <- raster::aggregate(
  raster_brasil, fact = fator_agregante)

# nova resolucao
raster::res(raster_brasil_baixaresolucao)[1]
raster::res(raster_brasil_baixaresolucao)[1] / (30 / 3600)


# salva o raster cortado e na nova resolu??o numa nova pasta
for (i in 1:nlayers(raster_brasil_baixaresolucao)) {
  raster::writeRaster(x = raster_brasil_baixaresolucao[[i]],
                      filename = paste0(
                        "input//raster_reduced//raster_br_res05_",
                        names(raster_brasil)[i]),
                      bylayer = TRUE,
                      options = c("COMPRESS=DEFLATE"),
                      format = "GTiff",
                      overwrite = TRUE)
}

# Avaliar a correla??o entre as vari?veis clim?ticas 
# cria uma nova pasta para a an?lise de correlacao
# extrai os valores
var_da <- raster_brasil_baixaresolucao %>%
  raster::values() %>%
  tibble::as_tibble() %>%
  tidyr::drop_na()

# correlacao - utilizaremos aqui o metodo de Spearman
cor_table <- corrr::correlate(var_da, method = "spearman")

# cria uma tabela com o resultado da correlacao
cor_table_summary <- cor_table %>%
  corrr::shave() %>%
  corrr::fashion()
View(cor_table_summary)

# e exporta a tabela
readr::write_csv(
  cor_table_summary, 
  "results/correlacao_variaveisambientais.csv")


# selecao das variaveis correlacionadas
fi_06 <- cor_table %>%
  corrr::as_matrix() %>%
  caret::findCorrelation(cutoff = .6, names = TRUE, verbose = TRUE)
fi_06


var_da_cor06 <- var_da %>%
  dplyr::select(-fi_06)
var_da_cor06

# verifica as variaveis correlacionadas
var_da_cor06 %>% 
  corrr::correlate(method = "spearman") %>%
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .6, names = TRUE, verbose = TRUE)

# grafico de correlacao 
tiff("results/correlacao_plot.tif", wi = 30, he = 25,
     un = "cm", 
     res = 300, 
     comp = "lzw")
pairs.panels(x = var_da_cor06 %>% dplyr::sample_n(1e3),
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




#### CONSIDERANDO OS RASTERS DO CMIP6 PARA O FUTURO ####
### PROBLEMA: ESTA ORGANIZADO EM BANDAS. POR ISSO, O CODIGO  PARA COPIAR O 
### RASTER DA PASTA 00 PARA A PASTA 03 NAO FUNCIONA DIREITO E OS RASTERS
### COPIADOS PARA A PASTA 03 NAO SAO OS SELECIONADOS PELA ANALISE DE CORRELACAO.
### PARA RESOLVER ISSO DE UMA MANEIRA RELATIVAMENTE RAPIDA, VOCE PODE
### SIMPLESMENTE SALVAR OS RASTERS SELECIONADOS UM A UM:

# setwd("03_var")
# writeRaster(raster_brasil_baixaresolucao[[3]], "wc14_br_res05g_miroc_ssp370_2070_prec_3.tif")

# altere o n?mero dentro dos colchetes de acordo com a posi??o da banda selecionada dentro do seu raster. para ver as posi??es, use:

# names(raster_brasil_baixaresolucao)


# fim da parte 1 :)

