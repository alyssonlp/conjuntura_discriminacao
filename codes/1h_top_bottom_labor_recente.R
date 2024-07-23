rm(list = ls()[which(!ls() %in% list_objects_to_keep)])
gc()

compute_stats = FALSE

if (compute_stats) {
  
  compute_percents <- function(aa, tri) {
    # aa = 2024
    # tri = 1
    print(paste0("Computing shares for year ", aa, ", quarter ", tri)) 
    
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
    
    # Calculando o top 10%
    top10 <- quantile(dt$r_hab_all, 0.90, na.rm = TRUE)
    # Calculando o top 5%
    top5 <- quantile(dt$r_hab_all, 0.95, na.rm = TRUE)
    # Calculando o top 1%
    top1 <- quantile(dt$r_hab_all, 0.99, na.rm = TRUE)
    
    # Calculando o bottom 10%
    bottom10 <- quantile(dt$r_hab_all, 0.10, na.rm = TRUE)
    # Calculando o bottom 5%
    bottom5 <- quantile(dt$r_hab_all, 0.05, na.rm = TRUE)
    # Calculando o bottom 1%
    bottom1 <- quantile(dt$r_hab_all, 0.01, na.rm = TRUE)
    
    # criando as variaveis dummies
    dt[, top10 := as.numeric(r_hab_all >= top10)]
    dt[, top5 := as.numeric(r_hab_all >= top5)]
    dt[, top1 := as.numeric(r_hab_all >= top1)]
    dt[, bottom10 := as.numeric(r_hab_all <= bottom10)]
    dt[, bottom5 := as.numeric(r_hab_all <= bottom5)]
    dt[, bottom1 := as.numeric(r_hab_all <= bottom1)]
    
    
    tb <- as.data.table(dist_fun(aa, tri))
    
    tb[,t10 := (top10/sum(top10))*100 ]
    tb[,t5 := (top5/sum(top5))*100 ]
    tb[,t1 := (top1/sum(top1))*100 ]
    tb[,b10 := (bottom10/sum(bottom10))*100 ]
    tb[,b5 := (bottom5/sum(bottom5))*100 ]
    tb[,b1 := (bottom1/sum(bottom1))*100 ]
    
    anotri <- sprintf("%dT%d", aa, tri)
    
    tb[, Ano_trimestre := anotri]
    
    tb <- tb[, .(gender_race, Ano_trimestre, t10, t5, t1, b10, b5, b1)]
    
    tb
    #dt1 <- rbind(dt1, tb, fill = TRUE)
    
  }
  
  resultado_2023 <- compute_percents(aa = 2023, tri = 1)
  resultado_2024 <- compute_percents(aa = 2024, tri = 1)
  
  resultado <- 
    lapply(c(2023, 2024), 
                      function(k) compute_percents(aa = k, tri = 1)) %>% 
    rbindlist()
  
  # 
  # dt1 <- dt1[, gender_race := gsub("_", " ", gender_race)]
  # dt1 <- dt1[, gender_race := gsub("homem", "Homem", gender_race)]
  # dt1 <- dt1[, gender_race := gsub("mulher", "Mulher", gender_race)]
  # dt1 <- dt1[, gender_race := gsub("negro", "Negro", gender_race)]
  # dt1 <- dt1[, gender_race := gsub("negra", "Negra", gender_race)]
  # dt1 <- dt1[, gender_race := gsub("branco", "Branco", gender_race)]
  # dt1 <- dt1[, gender_race := gsub("branca", "Branca", gender_race)]
  # 
  # fwrite(dt1, file.path(csv_files, "top_bottom.csv"))
  
  fwrite(resultado, file.path(csv_output, "top_bottom.csv"))
  
} else {
  
  results <- fread(file.path(csv_output, "top_bottom.csv"))
}

# criando o grafico

dt1_long <- results %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  tidyr::pivot_longer(cols = starts_with("b") | starts_with("t"), 
                      names_to = "category", 
                      values_to = "value")

dt1_long <- mutate(dt1_long, category_labels = case_when(
  category == "b1" ~ "base 1%",
  category == "b5" ~ "base 5%",
  category == "b10" ~ "base 10%",
  category == "t10" ~ "topo 10%",
  category == "t5" ~ "topo 5%",
  category == "t1" ~ "topo 1%",
  TRUE ~ category ))



dt1_long <- dt1_long %>%
  mutate(Ano_trimestre_category = 
           paste(Ano_trimestre, category_labels, sep = "-"))

dt1_long$Ano_trimestre_category <- as.factor(dt1_long$Ano_trimestre_category)

dt1_long$Ano_trimestre_category <- 
  forcats::fct_relevel(dt1_long$Ano_trimestre_category,
                       c("2023T1-base 1%", "2024T1-base 1%",
                         "2023T1-base 5%", "2024T1-base 5%",
                         "2023T1-base 10%", "2024T1-base 10%",
                         "2023T1-topo 10%", "2024T1-topo 10%",
                         "2023T1-topo 5%", "2024T1-topo 5%",
                         "2023T1-topo 1%", "2024T1-topo 1%"))


dt1_long <- dt1_long %>%
  mutate(base_topo = ifelse(grepl("b", Ano_trimestre_category), "Base", "Topo"))

dt1_long <- as.data.table(dt1_long)

dt1_long[, topo_base := as.numeric(category %in% c("b10", "b5", "b1"))]

dt1_long[, order := fcase(category %in% c("b1", "t10"), 1L,
                          category %in% c("b5", "t5"), 3L,
                          category %in% c("b10", "t1"), 5L)]

table(dt1_long$category, dt1_long$order)

dt1_long[Ano_trimestre == "2023T1", teste := order]
dt1_long[Ano_trimestre == "2024T1", teste := order + 1]

x_labels <- c("1" = "1%",
              "2" = "1%",
              "3" = "5%",
              "4" = "5%",
              "5" = "10%",
              "6" = "10%")

pdf(file.path(figures_output, "top_bottom.pdf"),  width = 14, height = 8.5)


tb <- ggplot(dt1_long, aes(x = teste, y = value, fill = gender_race)) +
  geom_bar(stat = 'identity', 
           position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(value)), 
            position = position_stack(vjust = 0.5), 
            size = 4.5) +
  scale_fill_manual(name = "",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = x_labels) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, lineheight = 0.9),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  labs(x = "", y = "%", title = "") +
  facet_grid

print(tb)
dev.off()
