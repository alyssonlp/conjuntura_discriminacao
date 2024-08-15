gen_raca <- fread(file.path(csv_output, "resultados_genero_raca_carta.csv"))
brasil <- fread(file.path(csv_output, "resultados_brasil_carta.csv"))
brasil[, gender_race := "Brasil"]
gen_raca <- gen_raca[, gender_race := gsub("_", " ", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("homem", "Homem", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("mulher", "Mulher", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negro", "Negro", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negra", "Negra", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branco", "Branco", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branca", "Branca", gender_race)]
br_gen_raca <- rbind(gen_raca, brasil)

# Incluindo dado para ficar invisivel:
brasil_inv <- brasil
brasil_inv[, gender_race := "Invisivel"]  
brasil_inv[, renda_media_hab := 0]  
brasil_inv[, renda_media_efe := 0]
brasil_inv[, massa_hab := 0]
brasil_inv[, massa_efe := 0]
brasil_inv[, tx_desocup := 0]
brasil_inv[, pea_fac := 0]
brasil_inv[, gini_hab := 0]
brasil_inv[, gini_efe := 0]
br_gen_raca <- rbind(br_gen_raca, brasil_inv)

breaks_seq <- c("Brasil",  "Invisivel", "Homem Branco", "Mulher Branca",
                "Homem Negro", "Mulher Negra") # altera a sequencia em que os grupos aparecem
# Ggplot preenche de cima pra baixo

labels_seq <- c("Brasil",  "", "Homem Branco", "Mulher Branca",
                "Homem Negro", "Mulher Negra") # Muda o rotulo na legenda

color_squeme <- c("Homem Branco" = "aquamarine4",
                  "Mulher Branca" = "darkorange1",
                  "Invisivel" = "transparent", # muda cor para transparente
                  "Homem Negro" = "darkgoldenrod1",
                  "Mulher Negra" = "brown4",
                  "Brasil" = "black")
linetype_squeme <- c("Homem Branco" ="solid",
                     "Mulher Branca" =  "solid", 
                     "Invisivel" = "solid",
                     "Homem Negro" = "solid", 
                     "Mulher Negra" ="solid",
                     "Brasil" = "solid")

# gph 1 - rendimento habitual médio
pdf(file.path(figures_output, "rendimento_habitual_br_gen_raca.pdf"),  width = 14, height = 8.5)
br_gen_raca_r_hab_all <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = renda_media_hab,
             color = gender_race, group = gender_race)) + 
  geom_line(linewidth = 3.0, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = color_squeme,
                     breaks = breaks_seq,
                     labels = labels_seq)+
  scale_linetype_manual(name = "",                      
                        values = linetype_squeme,
                        breaks = breaks_seq,
                        labels = labels_seq) +
  scale_x_discrete(breaks = c("2012T2", "2016T2", "2020T2","2024T2"), 
                   label = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 1000)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        legend.key.width = unit (1.5,"cm")) +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y = "R$",title = "")

print(br_gen_raca_r_hab_all)
dev.off()


# gph 2 - massa salarial real habitual 
gen_raca <- gen_raca[, massa_hab := massa_hab/1000000000]
gen_raca <- gen_raca %>%  
  filter(gender_race != "Brasil")

gen_raca$gender_race <- factor(gen_raca$gender_race, 
                               levels = c("Homem Branco", "Mulher Branca", 
                                          "Homem Negro", "Mulher Negra"))

pdf(file.path(figures_output, "massa_habitual_gen_raca.pdf"),  width = 14, height = 8.5)
gen_raca_massa_hab <- gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = massa_hab, color = gender_race, group = gender_race)) + 
  geom_line(linewidth = 3.0, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "aquamarine4",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "darkgoldenrod1",
                                "Mulher Negra" = "brown4")) +
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "solid", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="solid")) +
  scale_x_discrete(breaks = c("2012T2", "2016T2", "2020T2","2024T2"), 
                   label = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 25)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
  legend.key.width = unit (1.5,"cm"))+
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y = "Em R$ bilhões", title = "")

print(gen_raca_massa_hab)
dev.off()


# gph 3 - índice de gini - rendimento habitual de todos os trabalhos - renda individual e domicilIar
pdf(file.path(figures_output, "gini_br_gen_raca.pdf"),  width = 14, height = 8.5)
gini_br_gen_raca <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = gini_hab, color = gender_race, group = gender_race)) + 
  geom_line(linewidth = 3.0, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = color_squeme,
                     breaks = breaks_seq,
                     labels = labels_seq)+
  scale_linetype_manual(name = "",                      
                        values = linetype_squeme,
                        breaks = breaks_seq,
                        labels = labels_seq) +
  scale_x_discrete(breaks = c("2012T2", "2016T2", "2020T2","2024T2"), 
                   label = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0.40, 0.55), breaks = seq(0.40, 0.55, by = 0.05)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        legend.key.width = unit (1.5,"cm"))  +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y = "Índice de Gini", title = "")

print(gini_br_gen_raca)
dev.off()

# gph 4 - unemp
pdf(file.path(figures_output, "unemp_br_gen_raca.pdf"),  width = 14, height = 8.5)
unemp_br_gen_raca <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = tx_desocup*100, color = gender_race, group = gender_race)) + 
  geom_line(linewidth = 3.0, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = color_squeme,
                     breaks = breaks_seq,
                     labels = labels_seq)+
  scale_linetype_manual(name = "",                      
                        values = linetype_squeme,
                        breaks = breaks_seq,
                        labels = labels_seq) +
  scale_x_discrete(breaks = c("2012T2", "2016T2", "2020T2",  "2024T2"), 
                   label = c("2012","2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        legend.key.width = unit (1.5,"cm"))  +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y =  "%",
       title = "")

print(unemp_br_gen_raca)
dev.off()

# gph 5 - PEA
pdf(file.path(figures_output, "pea_br_gen_raca.pdf"),  width = 14, height = 8.5)
pea_br_gen_raca <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = pea_fac*100, color = gender_race, group = gender_race)) + 
  geom_line(linewidth = 3.0, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = color_squeme,
                     breaks = breaks_seq,
                     labels = labels_seq)+
  scale_linetype_manual(name = "",                      
                        values = linetype_squeme,
                        breaks = breaks_seq,
                        labels = labels_seq) +
  scale_x_discrete(breaks = c("2012T2", "2016T2", "2020T2","2024T2"), 
                   label = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        legend.key.width = unit (1.5,"cm"))  +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y =  "%", title = "")

print(pea_br_gen_raca)
dev.off()

# salvando
write.csv(br_gen_raca, file.path(csv_output, "descriptive_br_gender_race_carta.csv"))