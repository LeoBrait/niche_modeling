
# seleciona a planilha com a ocorrencia das especies
dados_ocorrencia <- read.csv("dados/ocorrencias/soldadinho.csv", header = TRUE)

# exclui linhas onde Lat ou Lon estao ausentes (NA)
dados_ocorrencia <- tidyr::drop_na(dados_ocorrencia, lon, lat)

# 2. para fins didaticos, os passos de carregamento e limpeza dos dados
# foram feitos em dois blocos de códigos, mas podem ser simplificados em
# um único bloco.
# o comando %>% eh utilizado para encadear as funcoes, basicamente o que ele diz
# eh "pegue o resultado dessa linha e jogue como primeiro argumento
# da proxima linha":
dados_ocorrencia <- read.csv(
  "dados/ocorrencias/soldadinho.csv", header = TRUE) %>%
    tidyr::drop_na(lon, lat)

# limpeza de coordenadas!
# primeiro marca os pontos problematicos
flags_spatial <- CoordinateCleaner::clean_coordinates(
    x = dados_ocorrencia,
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


# excluir os pontos marcados como problematicos
dados_ocorrencia_limpos <- dados_ocorrencia %>%
    dplyr::filter(flags_spatial$.summary == TRUE)

# resumo dos dados
dados_ocorrencia$species %>% table
dados_ocorrencia_limpos$species %>% table


raster_de_referencia <- raster_brasil_baixaresolucao[[1]]

#Question: What is happening here?
raster_de_referencia[!is.na(raster_de_referencia)] <- raster::cellFromXY(
  raster_de_referencia,
  raster::rasterToPoints(raster_de_referencia)[, 1:2])


# landscapetools::show_landscape(raster_de_referencia) +
#     geom_polygon(
#       data = raster_de_referencia %>%
#         raster::rasterToPolygons() %>%
#         fortify,
#       aes(x = long, y = lat, group = group),
#       fill = NA,
#       color = "black",
#       size = .1) +
#     theme(legend.position = "none")

# associa os dados de distribuicao apos limpeza com os rasters climaticos
# selecionados e filtra a distribucao com base nos dados taxonomicos (sp)
tabela_referencia <- dados_ocorrencia_limpos %>%
    dplyr::mutate(oppc = raster::extract(raster_de_referencia, dplyr::select(., lon, lat))) %>% #nolint
    dplyr::filter(!is.na(oppc)) %>%
    dplyr::add_count(species) %>%
    dplyr::arrange(species)
View(tabela_referencia)

