# -------------------------------------------------------------------------
### Modelagem de nicho ecol?gico: Teoria e pr?tica 
# Limpeza/filtragem dos dados 

# Profas. Juliana Hipólito e Luisa Maria Diele-Viegas 
# -------------------------------------------------------------------------


# seleciona a planilha com a ocorrencia das especies
dados_ocorrencia <- read.csv("input/fulanus.csv", header = TRUE)

# exclui linhas onde Lat ou Lon estao ausentes (NA)
dados_ocorrencia <- tidyr::drop_na(dados_ocorrencia, lon, lat)

# 2. para fins didaticos, os passos de carregamento e limpeza dos dados
# foram feitos em dois blocos de códigos, mas podem ser simplificados em
# um único bloco.
# o comando %>% eh utilizado para encadear as funcoes, basicamente o que ele diz
# eh "pegue o resultado dessa linha e jogue como primeiro argumento
# da proxima linha":
dados_ocorrencia <- read.csv("input/fulanus.csv", header = TRUE) %>%
    tidyr::drop_na(lon, lat)



# limpeza de coordenadas! 
# primeiro marca os pontos problematicos
flags_spatial <- CoordinateCleaner::clean_coordinates(
    x = dados_ocorrencia,
    species = "species",
    lon = "lon",
    lat = "lat",
    tests = c("capitals", # 10km raio ao redor de capitais
              "centroids", # 1km raio ao redor de centroides de paises e provincias
              "duplicates", # duplicatas
              "equal", # coordenadas iguais
              "gbif", # raio ao redor da sede da GBIF
              "institutions", # raio ao redor de instituicoes de pesquisa em biodiversidade
              "seas", # pontos no mar
              "urban", # pontos dentro de areas urbanas
              "validity", # ponto de fora do sistema de coordenadas
              "zeros" # zeros e pontos onde lat = lon 
    )
  )

  # resultado da marcacao de 'pontos problematicos'
  #' TRUE = coordenadas 'limpas'
  #' FALSE = coordenadas potencialmente problematicas

flags_spatial %>% head
  summary(flags_spatial)

  # excluir os pontos marcados como problematicos
occ_data_tax_date_spa <- dados_ocorrencia %>% 
    dplyr::filter(flags_spatial$.summary == TRUE)
  occ_data_tax_date_spa

  # resumo dos dados
  dados_ocorrencia$species %>% table
  occ_data_tax_date_spa$species %>% table

### TODO: por que apenas um raster?
var_id <- raster::raster(
    "input/raster_reduced/raster_br_res05_wc2.1_5m_bio_3.tif")

  
  var_id[!is.na(var_id)] <- raster::cellFromXY(var_id, raster::rasterToPoints(var_id)[, 1:2])
  landscapetools::show_landscape(var_id) +
    geom_polygon(data = var_id %>% raster::rasterToPolygons() %>% fortify, 
                 aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
    theme(legend.position = "none")
  
  # associa os dados de distribuicao apos limpeza com os rasters climaticos 
  # selecionados e filtra a distribucao com base nos dados taxonomicos (sp)
  occ_data_tax_date_spa_oppc <- occ_data_tax_date_spa %>% 
    dplyr::mutate(oppc = raster::extract(var_id, dplyr::select(., lon, lat))) %>% 
    #dplyr::distinct(species, oppc, .keep_all = TRUE) %>%  
    dplyr::filter(!is.na(oppc)) %>% 
    dplyr::add_count(species) %>% 
    dplyr::arrange(species)
  occ_data_tax_date_spa_oppc
  
  
  # verifica 
  table(occ_data_tax_date_spa$species)
  table(occ_data_tax_date_spa_oppc$species)
  
  occ_data_tax_date_spa$species %>% table
  occ_data_tax_date_spa_oppc$species %>% table
  
  #exporta a planilha para uma nova pasta no diretorio de trabalho
  # cria a pasta a ser utilizada
  
  # exporta a planilha em csv
write_csv(occ_data_tax_date_spa_oppc, 
  "input/ocorrencias_bioticas_abioticas.csv")
  
  
  # Fim do script 2 :)