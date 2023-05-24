occ <- read_csv("input/ocorrencias_bioticas_abioticas.csv")

tif_files_paths <- dir(
  "input/raster_reduced",
  pattern = ".tif",
  full.names = TRUE)
var <- raster::stack(tif_files_paths) %>%
  raster::brick()


# definindo os parametros a priori
replica <- 10 #numeros de replicas que serao usadas no modelo
partition <- .7 #definindo j? aqui que serao 70% treino, 30% teste!

# algoritmos = vamos usar um loop para rodar todos de uma vez!
for (i in unique(occ$species)){ # para cada especie

  # objeto para avaliacao
  eval_species <- tibble::tibble()

  # selecionando dados de presenca e (pseudo)ausencia
  # dados de presenca
  present_species <- occ %>%
    filter(species == i) %>%
    dplyr::select(lon, lat) %>%
    mutate(id = seq(nrow(.)))

  # dados de pseudoausencia
  pabsent_species <- dismo::randomPoints(
    mask = var,
    n = nrow(present_species)  *10) %>% #*10 quando N de pontos for baixo
    tibble::as_tibble() %>%
    dplyr::rename(lon = x, lat = y) %>%
    dplyr::mutate(id = seq(nrow(.)))

  pabsent_species_glm <- dismo::randomPoints(mask = var, n = 10000) %>%
    tibble::as_tibble() %>%
    dplyr::rename(lon = x, lat = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))


  # replicas
  for (r in replica %>% seq){	# numero de replicas do modelo 

    # objeto para a avaliacao
    eval_algorithm <- tibble::tibble()

    # particionando os dados com base na nossa selecao
    # dados de presenca
    present_sample_train <- present_species %>%
      dplyr::sample_frac(partition) %>%
      dplyr::select(id) %>%
      dplyr::pull()

    # dados de pseudo ausencia
    pabsent_sample_train <- pabsent_species %>%
      dplyr::sample_frac(partition) %>%
      dplyr::select(id) %>%
      dplyr::pull()

    # dados de pseudo ausencia
    pabsent_sample_train_glm <- pabsent_species_glm %>%
      dplyr::sample_frac(partition) %>%
      dplyr::select(id) %>%
      dplyr::pull()
    gc()
    # dados de treino e teste !
    # treino
    train <- dismo::prepareData(
      x = var,
      p = present_species %>%
          dplyr::filter(id %in% present_sample_train) %>%
          dplyr::select(lon, lat),

      b = pabsent_species %>%
          dplyr::filter(id %in% pabsent_sample_train) %>%
          dplyr::select(lon, lat)) %>%
          na.omit

    # teste
    test <- dismo::prepareData(
            x = var,
            p = present_species %>%
                  dplyr::filter(!id %in% present_sample_train) %>%
                  dplyr::select(lon, lat),

            b = pabsent_species %>%
                dplyr::filter(!id %in% pabsent_sample_train) %>%
                dplyr::select(lon, lat)) %>%
                na.omit

    train_glm <- dismo::prepareData(
      x = var,
      p = present_species %>%
          dplyr::filter(id %in% present_sample_train) %>%
          dplyr::select(lon, lat),
      
      b = pabsent_species %>%
          dplyr::filter(id %in% pabsent_sample_train_glm) %>%
          dplyr::select(lon, lat)) %>%
          na.omit
    
    # teste
    test_glm <- dismo::prepareData(x = var, 
                                   p = present_species %>% 
                                     dplyr::filter(!id %in% present_sample_train) %>% 
                                     dplyr::select(lon, lat), 
                                   b = pabsent_species %>% 
                                     dplyr::filter(!id %in% pabsent_sample_train_glm) %>% 
                                     dplyr::select(lon, lat)) %>% na.omit
    
    gc()
    
    # Ajuste do modelo
    # informacao
    print(paste("Models fitting to", i, "replica", r, "of", replica))
    
    # Agora vamos de diferentes algoritmos!
    
    # Envelope climatico - Somente dados de presenca!
    BIO <- dismo::bioclim(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # Distancia ambiental - somente dados de presenca!
    # DOM <- dismo::domain(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    # MAH <- dismo::mahal(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # Regressao - Modelo linear generalizado - dados de presenca e (pseudo)ausencia! 
    GLM <- glm(formula = pb ~ ., family = binomial(link = "logit"), data = train_glm) 
    #family=c("binomial","gaussian","poisson")
    
    # SVM (machine learning) - Dados de presen?a e plano de fundo
    SVM <- kernlab::ksvm(x = pb ~ ., data = train)
    # Lista com todos os algoritmos :)
    fit <- list(bioclim = BIO,  glm = GLM, svm = SVM) # domain = DOM, mahalanobis = MAH #
    gc()
    # Previsoes (mais uma vez no loop)
    for(a in seq(fit)){
      
      # informacao 
      print(paste("Model predict algorithm", fit[a] %>% names))
      
      # previsao do modelo 
      model_predict <- dismo::predict(var, fit[[a]], progress = "text")
      
      # exporta os valores da previsao do modelo 
      raster::writeRaster(x = model_predict, 
                          filename = paste0("results/predictions/", "enm_", i, "_", fit[a] %>% names,
                                            "_r", ifelse(r < 10, paste0("0", r), r)), 
                          format = "GTiff", 
                          options = c("COMPRESS=DEFLATE"), 
                          overwrite = TRUE)
      
      # avaliacao do modelo
      eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                              a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                              model = fit[[a]])
      gc()
      # indices de avaliacao 
      id_eval_spec_sens <- which(eval@t == dismo::threshold(eval, "spec_sens"))
      tss_spec_sens <- eval@TPR[id_eval_spec_sens] + eval@TNR[id_eval_spec_sens] - 1
      
      # dados da avaliacao 
      eval_data <- tibble::tibble(species = i, 
                                  replica = r, 
                                  algorithm = fit[a] %>% names, 
                                  thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
                                  tss_spec_sens = tss_spec_sens,
                                  auc = eval@auc, 
                                  file = paste0("results/predictions/", "enm_", i, "_", fit[a] %>% names,
                                                "_r", ifelse(r < 10, 
                                                             paste0("0", r), r), ".tif"))

      # combina a avaliacao dos modelos 
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)

    } 

    # combina as avaliacoes
    eval_species <- dplyr::bind_rows(eval_species, eval_algorithm)

  }


  readr::write_csv(eval_species, paste0("results/predictions/", "eval_", i, ".csv"))

}

# Arrasamos em dobro! Fim :)
