rasters <- dir(
  "dados/raster_reduced", pattern = ".tif", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()

# definindo os parametros a priori
replica <- 1 #numeros de replicas que serao usadas no modelo
partition <- .7 #definindo aqui que serao 70% treino, 30% teste!

# algoritmos = vamos usar um loop para rodar todos de uma vez!
for (i in unique(tabela_referencia$species)){ # para cada especie

  # cria uma tabela vazia
  especies_avaliadas <- tibble::tibble()

  # dados de presenca
  present_species <- tabela_referencia %>%
    filter(species == i) %>%
    dplyr::select(lon, lat) %>%
    mutate(id = seq(nrow(.)))

  # dados de pseudoausencia
  pabsent_species <- dismo::randomPoints(mask = rasters,
    n = nrow(present_species) * 10) %>%
    tibble::as_tibble() %>%
    dplyr::rename(lon = x, lat = y) %>%
    dplyr::mutate(id = seq(nrow(.)))

  pabsent_species_glm <- dismo::randomPoints(mask = rasters, n = 1000) %>%
    tibble::as_tibble() %>%
    dplyr::rename(lon = x, lat = y) %>%
    dplyr::mutate(id = seq(nrow(.)))


  # replicas
  for (r in seq(replica)){	# numero de replicas do modelo

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
      x = rasters,
      p = present_species %>%
          dplyr::filter(id %in% present_sample_train) %>%
          dplyr::select(lon, lat),

      b = pabsent_species %>%
          dplyr::filter(id %in% pabsent_sample_train) %>%
          dplyr::select(lon, lat)) %>%
          na.omit

    # teste
    test <- dismo::prepareData(
            x = rasters,
            p = present_species %>%
                  dplyr::filter(!id %in% present_sample_train) %>%
                  dplyr::select(lon, lat),

            b = pabsent_species %>%
                dplyr::filter(!id %in% pabsent_sample_train) %>%
                dplyr::select(lon, lat)) %>%
                na.omit()

    train_glm <- dismo::prepareData(
      x = rasters,
      p = present_species %>%
          dplyr::filter(id %in% present_sample_train) %>%
          dplyr::select(lon, lat),

      b = pabsent_species %>%
          dplyr::filter(id %in% pabsent_sample_train_glm) %>%
          dplyr::select(lon, lat)) %>%
          na.omit()

    # teste
    test_glm <- dismo::prepareData(
      x = rasters,
      p = present_species %>%
          dplyr::filter(!id %in% present_sample_train) %>%
          dplyr::select(lon, lat),

      b = pabsent_species %>%
          dplyr::filter(!id %in% pabsent_sample_train_glm) %>%
          dplyr::select(lon, lat)) %>%
          na.omit()
    gc()

    # Ajuste do modelo
    # informacao
    print(paste("Models fitting to", i, "replica", r, "of", replica))

    # Agora vamos de diferentes algoritmos!
    # Envelope climatico - Somente dados de presenca!
    BIO <- dismo::bioclim(
      x = train %>%
          dplyr::filter(pb == 1) %>%
          dplyr::select(-pb))

    # Distancia ambiental - somente dados de presenca!
    # DOM <- dismo::domain(x = train %>% dplyr::filter(pb == 1)
    # %>% dplyr::select(-pb))
    # MAH <- dismo::mahal(x = train %>% dplyr::filter(pb == 1) %>%
    # dplyr::select(-pb))

    # Regressao - Modelo linear generalizado
    GLM <- glm(
      formula = pb ~ .,
      family = binomial(link = "logit"),
      data = train_glm)
    #family = c("binomial","gaussian","poisson") #nolint

    # SVM (machine learning) - Dados de presen?a e plano de fundo
    SVM <- kernlab::ksvm(x = pb ~ ., data = train)

    fit <- list(bioclim = BIO,  glm = GLM, svm = SVM)
    gc()
    # Previsoes (mais uma vez no loop)
    for (a in seq(fit)){

      # informacao
      print(paste("Model predict algorithm", fit[a] %>% names))

      # previsao do modelo
      model_predict <- dismo::predict(rasters, fit[[a]], progress = "text")

      # exporta os valores da previsao do modelo
      raster::writeRaster(
        x = model_predict,
        filename = paste0(
          "results/predictions/",
          "enm_",
          i,
          "_",
          names(fit[a]),
          "_r",
          ifelse(r < 10, paste0("0", r),
          r)),
        format = "GTiff",
        options = c("COMPRESS=DEFLATE"),
        overwrite = TRUE)

      # avaliacao do modelo
      eval <- dismo::evaluate(
        p = test %>%
          dplyr::filter(pb == 1) %>%
          dplyr::select(-pb),
        a = test %>%
          dplyr::filter(pb == 0) %>%
          dplyr::select(-pb),
        model = fit[[a]])

      gc()

      # indices de avaliacao
      id_eval_spec_sens <- which(eval@t == dismo::threshold(eval, "spec_sens"))
      tss_spec_sens <- eval@TPR[id_eval_spec_sens] +
        eval@TNR[id_eval_spec_sens] - 1

      # dados da avaliacao
      eval_data <- tibble::tibble(
        species = i,
        replica = r,
        algorithm = names(fit[a]),
        thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
        tss_spec_sens = tss_spec_sens,
        auc = eval@auc,
        file = paste0("results/predictions/",
          "enm_",
          i,
          "_",
          names(fit[a]),
          "_r",
          ifelse(r < 10, paste0("0", r), r),
          ".tif"))

      # combina a avaliacao dos modelos
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)

    }

    # combina as avaliacoes
    especies_avaliadas <- dplyr::bind_rows(especies_avaliadas, eval_algorithm)
  }

readr::write_csv(
  especies_avaliadas,
  paste0("results/predictions/", "eval_", i, ".csv"))
}

# Arrasamos em dobro! Fim :)
