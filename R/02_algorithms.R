#' @Author: Luísa Viegas, Leonardo Brait
#' @description
#' @requirements R 4.3.1 Ubuntu 20.04


################################# Environment ##################################
library("tidyverse") #2.0.0
library("sf") #1.0.14
library("raster") #3.6.23
library("dismo") #1.3.14
library("kernlab") #0.9.32

# settings -----------------------------
replica <- 100
partition <- .7

################################## Load Data ###################################
rasters <- dir(
  "data/raster_processed", pattern = ".tif", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()
occurencies <- readr::read_csv("data/occurrencies/soldadinho_clean.csv")

################################## Algorithms ##################################
if (!dir.exists("data/predictions")) {
  dir.create("data/predictions")
}

# presence and pseudo absence tables
present_species <- occurencies %>%
  dplyr::select(lon, lat) %>%
  mutate(id = seq(nrow(.)))
pabsent_species <- 
  dismo::randomPoints(mask = rasters, n = nrow(present_species) * 10) %>%
  tibble::as_tibble() %>%
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(id = seq(nrow(.)))
pabsent_species_glm <- dismo::randomPoints(mask = rasters, n = 1000) %>%
  tibble::as_tibble() %>%
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(id = seq(nrow(.)))

especies_avaliadas <- tibble::tibble()
for (r in seq(replica)){
  eval_algorithm <- tibble::tibble()

  # Create train and test data -------------------------------------------------
  ## ids for train and test
  ids_presence_train <- present_species %>%
    dplyr::sample_frac(partition) %>%
    dplyr::select(id) %>%
    dplyr::pull()
  ids_pabsent_train <- pabsent_species %>%
    dplyr::sample_frac(partition) %>%
    dplyr::select(id) %>%
    dplyr::pull()
  ids_pabsent_train_glm <- pabsent_species_glm %>%
    dplyr::sample_frac(partition) %>%
    dplyr::select(id) %>%
    dplyr::pull()
    gc()
  ## train and test data tables
  train <- dismo::prepareData(
    x = rasters,
    p = present_species %>%
        dplyr::filter(id %in% ids_presence_train) %>%
        dplyr::select(lon, lat),
    b = pabsent_species %>%
        dplyr::filter(id %in% ids_pabsent_train) %>%
        dplyr::select(lon, lat)
    ) %>%
    na.omit()
  test <- dismo::prepareData(
    x = rasters,
    p = present_species %>%
        dplyr::filter(!id %in% ids_presence_train) %>%
        dplyr::select(lon, lat),
    b = pabsent_species %>%
        dplyr::filter(!id %in% ids_pabsent_train) %>%
        dplyr::select(lon, lat)) %>%
        na.omit()
  ## train and test data tables for glm
  train_glm <- dismo::prepareData(
    x = rasters,
    p = present_species %>%
      dplyr::filter(id %in% ids_presence_train) %>%
      dplyr::select(lon, lat),
    b = pabsent_species %>%
      dplyr::filter(id %in% ids_pabsent_train_glm) %>%
      dplyr::select(lon, lat)) %>%
      na.omit()
  test_glm <- dismo::prepareData(
      x = rasters,
      p = present_species %>%
          dplyr::filter(!id %in% ids_presence_train) %>%
          dplyr::select(lon, lat),
      b = pabsent_species_glm %>%
          dplyr::filter(!id %in% ids_pabsent_train_glm) %>%
          dplyr::select(lon, lat)) %>%
          na.omit()
    gc()
  
  # run algorithms -------------------------------------------------------------
  print(paste("Models fitting for replica", r, "of", replica))

  BIO <- dismo::bioclim(
    x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb)
  )
  GLM <- glm(
    formula = pb ~ .,
    family = binomial(link = "logit"),
    data = train_glm
  )
  SVM <- kernlab::ksvm(x = pb ~ ., data = train)
  fit <- list(bioclim = BIO,  glm = GLM, svm = SVM)
  gc()

  # predict and evaluate -------------------------------------------------------
  for (a in seq(fit)){
    print(paste("Model predict algorithm", fit[a] %>% names))
    
    model_predict <- dismo::predict(rasters, fit[[a]], progress = "text")
    
    # Save data
    raster::writeRaster(
        x = model_predict,
        filename = paste0("data/predictions/", names(fit[a]), r),
        format = "GTiff",
        options = c("COMPRESS=DEFLATE"),
        overwrite = TRUE
    )

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

      # Save data
      eval_data <- tibble::tibble(
        replica = r,
        algorithm = names(fit[a]),
        thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
        tss_spec_sens = tss_spec_sens,
        auc = eval@auc,
        file = paste0("data/predictions/", names(fit[a]), r, ".tif")
      )
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)
  }
    especies_avaliadas <- dplyr::bind_rows(especies_avaliadas, eval_algorithm)
}

readr::write_csv(
  especies_avaliadas,
  paste0("data/predictions/", "scores", ".csv"))
