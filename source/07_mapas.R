# -------------------------------------------------------------------------
### Modelagem de nicho ecologico: Teoria e pratica 
# mapas! 

# Prof. Luisa Maria Diele-Viegas e Juliana Hipolito
# -------------------------------------------------------------------------





occ <- tabela_referencia

br <- poligono_geografico




# Plotando os graficos!

#for(i in occ$species %>% unique){
  
  # informacao 
  sp <- i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")
  print(sp)
  
  
  # importando os dados de consenso
  ens <- dir("results/ensemble", pattern = "presente_", recursive = TRUE, full.names = TRUE) %>% 
    stringr::str_subset(".tif$") %>% 
    raster::stack()
  
  # ens <- dir("results/nichos_modelados", pattern = "consenso_cortadop20", recursive = TRUE, full.names = TRUE) %>% 
  #   stringr::str_subset(".tif$") %>% 
  #   raster::stack()
j <- 1
  # gerando os mapas!
      #for(j in ens %>% raster::nlayers() %>% seq){
        foo <- names(ens)[j]
        foo <- gsub("presente_","",foo)
        foo <- gsub("_"," ",foo)
        #if (foo == i) {
          occ_temp<- occ%>% filter(species == i)
          print(c(i, j))
        
          map <- ggplot() +
          geom_raster(data = raster::rasterToPoints(ens[[j]]) %>% tibble::as_tibble() %>% 
                        dplyr::rename(ens = names(ens[[j]])),
                      aes(x, y, fill = ens)) +
          #geom_sf(data = br, fill = NA, color = "gray30", geometry = geometry) +
            geom_point(data = occ_temp , aes(lon, lat, group = occ_temp$species %>%
                                               str_to_title() %>% sub("_", " ", .)),  
                       size = 2, alpha = .7) +
            scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
            scale_fill_gradientn(colours = rev(wesanderson::wes_palette("Zissou1", n = 100, type = "continuous"))) +
            # coord_sf(xlim = c(-90, -25)) +
            labs(x = "Longitude", y = "Latitude", fill = "Adequability", color = "Occurrence", 
                 title = bquote(bold(bolditalic(.(names(ens[[j]]) %>%  str_to_title() %>% sub("_", " ", .)))))) +
            annotation_scale(location = "br", width_hint = .3) +
            annotation_north_arrow(location = "br", which_north = "true", 
                                   pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                                   style = north_arrow_fancy_orienteering) +
          theme(title = element_text(size = 12, face = "bold"),
                legend.title = element_text(size = 8, face = "bold"),
                legend.background = element_rect(fill = "white",
                                                 size = 0.3, 
                                                 linetype = "solid", 
                                                 colour = "black"),
                axis.title = element_text(size = 15, face = "plain"))#,
                #legend.position = c(.1, .15))
        map
        
        # exportando os mapas
        ggsave(paste0("results/Presente_",i, ".tiff"), map, wi = 20, he = 20, un = "cm", dpi = 300, comp = "lzw") 
        # inserir pro paste0 se necessario ,names(ens[[j]])
       #       }
    
      #}
  
  