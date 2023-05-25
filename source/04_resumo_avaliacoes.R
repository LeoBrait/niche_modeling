# -------------------------------------------------------------------------
### Modelagem de nicho ecologico: Teoria e pratica 
# avaliacao: tabelas e boxplot

# Prof. Luisa Maria Diele-Viegas e Juliana Hipolito
# -------------------------------------------------------------------------

# importando os dados da avaliacao
avaliacoes <- dir(
  "results/predictions",
  pattern = "eval_",
  recursive = TRUE,
  full.names = TRUE) %>%
  read_csv()


# agora vamos avaliar a analise que fizemos
for (i in avaliacoes$species %>% unique){

  # seleciona a especie
  avaliacoes_sp <- avaliacoes %>%
    dplyr::filter(species == i)

  # tabela para avaliar os modelos por TSS e AUC
  avaliacoes_table <- avaliacoes_sp %>%
    dplyr::group_by(species, algorithm) %>%
    dplyr::summarise(
      tss_mean = mean(tss_spec_sens),
      tss_sd = sd(tss_spec_sens),
      auc_mean = mean(auc),
      auc_sd = sd(auc))

  # exportando a avaliacao dos modelos
  #dado de consumo interno
  readr::write_csv(
    avaliacoes_table,
    paste0("results/predictions/avaliacao_sumarizadas", i, ".csv"))

  # boxplots

  for (j in c("tss_spec_sens", "auc")){

    # informacao
    print(paste(i, j))

    # plot dos boxplots referentes aos diferentes algoritmos
    ggplot(data = avaliacoes_sp) +

      #tema
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold.italic", size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 17)) +
      labs(x = "Algorithms") +
      ylim(c(-.01, 1.05)) +

      #limiares
      aes_string(x = "algorithm", y = j, color = "algorithm") +
      geom_hline(
        yintercept = ifelse(j == "tss_spec_sens", .5, .8),
        color = "red") +

      #caixas
      geom_boxplot(size = .5, fill = "gray90", color = "black") +

      #pontos
      geom_jitter(width = 0.2, size = 4, alpha = .7)

    ggsave(
      paste0("results/boxplot_", j, "_", i, ".png"),
      he = 20,
      wi = 30,
      un = "cm",
      dpi = 300)

  }

}

# Fim! :)

