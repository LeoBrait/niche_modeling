# Define o poligono que sera utilizado para extrair valores de adequabilidade.
# a ?rea de interesse utilizada foi o Brasil.
br <- rnaturalearth::ne_countries(country = "Brazil", returnclass = "sf")

setwd(path)
setwd("05_consenso")

# importando o raster de consenso
ens <- dir(pattern = "presente", recursive = TRUE) %>%
  stringr::str_subset(".tif$") %>%
  raster::stack()


# AJUSTE DO RASTER PARA ESCALA 0-1

objeto <- ens[[1]]/maxValue(ens[[1]])


# definindo valores de adequabilidade termica dentro do poligono.
# no caso do exemplo, estamos recuperando os valores por pixel 
adeq_present <- raster::extract(objeto, br)



#salva os valores em csv
write.csv(adeq_present, file = "adequability.csv")