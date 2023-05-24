# -------------------------------------------------------------------------
### Modelagem de nicho ecologico: Teoria e pratica 
# avaliacao: tabelas e boxplot

# Prof. Luisa Maria Diele-Viegas e Juliana Hipolito
# -------------------------------------------------------------------------

# importando os dados da avaliacao
eva <- purrr::map_df(dir(pattern = "eval_", recursive = TRUE), readr::read_csv)

# agora vamos avaliar a analise que fizemos 
for(i in eva$species %>% unique){

  
  # seleciona a especie 
  eva_sp <- eva %>% 
    dplyr::filter(species == i)

  
  # tabela para avaliar os modelos por TSS e AUC
  eva_table <- eva_sp %>% 
    dplyr::mutate(species = species %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) %>% 
    dplyr::group_by(species, algorithm) %>% 
    dplyr::summarise(tss_mean = mean(tss_spec_sens) %>% round(3), 
                     tss_sd = sd(tss_spec_sens) %>% round(3),
                     auc_mean = mean(auc) %>% round(3), 
                     auc_sd = sd(auc) %>% round(3))
  eva_table
  
  # exportando a avaliacao dos modelos
  readr::write_csv(eva_table, paste0("results/script04/evaluation_summary_table_", i, ".csv"))
  
  # boxplots
  
  for(j in c("tss_spec_sens", "auc")){
    
    # informacao
    print(paste(i, j))
    
    # plot dos boxplots referentes aos diferentes algoritmos  
    ggplot(data = eva_sp) + 
      aes_string(x = "algorithm", y = j, color = "algorithm") +
      geom_boxplot(size = .5, fill = "gray90", color = "black") +
      geom_jitter(width = 0.2, size = 4, alpha = .7) +
      scale_color_manual(values = wesanderson::wes_palette(name = "Darjeeling1", n = eva$algorithm %>% unique %>% length, 
                                                           type = "continuous")) +
      labs(x = "Algorithms", 
           y = stringr::str_to_upper(j) %>% stringr::str_replace("_", " "), 
           title = i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) + 
      ylim(c(-.01, 1.05)) + 
      theme_bw() +
      geom_hline(yintercept = ifelse(j == "tss_spec_sens", .5, .8), color = "red") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold.italic", size = 20), 
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 15), 
            axis.title = element_text(size = 17))
    ggsave(paste0("results/script04/boxplot_jitter_an_", j, "_", i, ".tiff"), he = 20, wi = 30, un = "cm", dpi = 300)
    
  }
  
}




# Fim! :)

