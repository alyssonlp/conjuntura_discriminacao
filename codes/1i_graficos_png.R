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

# rendimento habitual médio
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
        text = element_text(size = 26),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        legend.key.width = unit (1.5,"cm")) +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y = "R$", title = "Salário Médio, 25-65 anos", 
       caption = "Fonte: PNAD Contínua, IBGE")
ggsave(file.path(figures_output, "rendimento_habitual_medio.png"), plot = br_gen_raca_r_hab_all, 
       width = 12, height = 8, dpi = 500)



#  unemp
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
        text = element_text(size = 26),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        legend.key.width = unit (1.5,"cm"))  +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y =  "%", title = "Taxa de Desemprego, 25-65 anos",
       caption = "Fonte: PNAD Contínua, IBGE")
ggsave(file.path(figures_output, "desemprego.png"), plot = unemp_br_gen_raca, 
       width = 12, height = 8, dpi = 500)

# gini
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
  labs(x = "", y = "Índice de Gini", title = "índice de Gini, 25-65 anos",
       caption = "Fonte: PNAD Contínua, IBGE")

ggsave(file.path(figures_output, "gini.png"), plot = gini_br_gen_raca, 
       width = 12, height = 8, dpi = 500)

#  PEA

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
  labs(x = "", y =  "%", title = "População Economicamente Ativa, 25-65 anos",
       caption = "Fonte: PNAD Contínua, IBGE")
ggsave(file.path(figures_output, "pea.png"), plot = pea_br_gen_raca, 
       width = 12, height = 8, dpi = 500)

dt1 <- br_gen_raca %>%  
  filter(Ano_trimestre %in% c("2023T2", "2024T2"))

breaks_seq <- c("Brasil",  "Invisivel", "Homem Branco", "Mulher Branca",
                "Homem Negro", "Mulher Negra") 

labels_seq <- c("Brasil",  "", "Homem Branco", "Mulher Branca",
                "Homem Negro", "Mulher Negra") 

color_squeme <- c("Homem Branco" = "aquamarine4",
                  "Mulher Branca" = "darkorange1",
                  "Invisivel" = "transparent",
                  "Homem Negro" = "darkgoldenrod1",
                  "Mulher Negra" = "brown4",
                  "Brasil" = "black")

linetype_squeme <- c("Homem Branco" ="solid",
                     "Mulher Branca" =  "solid", 
                     "Invisivel" = "solid",
                     "Homem Negro" = "solid", 
                     "Mulher Negra" ="solid",
                     "Brasil" = "solid")

# renda habitual media

r_hab_all <- dt1 %>% 
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), 
             y = renda_media_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = ifelse(renda_media_hab == 0, "", round(renda_media_hab))), 
            vjust = -0.5,
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "", 
                    values = color_squeme,
                    breaks = breaks_seq,
                    labels = labels_seq,
                    drop = FALSE) + 
  scale_x_discrete(limits = c("2023T2.Brasil", "2024T2.Brasil",
                              "2023T2.Homem Branco", "2024T2.Homem Branco",
                              "2023T2.Mulher Branca", "2024T2.Mulher Branca",
                              "2023T2.Homem Negro", "2024T2.Homem Negro",
                              "2023T2.Mulher Negra", "2024T2.Mulher Negra"),
                   labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = "2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024",
                              "2023T2.Brasil" = "2023",
                              "2024T2.Brasil" = "2024",
                              "2023T2.Invisivel" = "",
                              "2024T2.Invisivel" = "")) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 1000)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 26),
        legend.position = "bottom",
        legend.title = element_text(size = 34),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 34, margin = margin(b = 20)), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, margin = margin(t = 10, b = 10)), 
        legend.text = element_text(size = 28),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  labs(x = "", y = "R$", title = "Salário Médio",
       subtitle = "2º trimestre, 25-65 anos",
       caption = "Fonte: PNAD Contínua, IBGE")
  ggsave(file.path(figures_output, "rendimento_habitual_medio_atual.png"), plot = r_hab_all, 
         width = 12, height = 8, dpi = 500)


# taxa de desemprego
unemp <- dt1 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race),
             y = 100*tx_desocup, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(100*tx_desocup,2)), vjust = -0.5, 
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "", 
                    values = color_squeme,
                    breaks = breaks_seq,
                    labels = labels_seq,
                    drop = FALSE) + 
  scale_x_discrete(limits = c("2023T2.Brasil", "2024T2.Brasil",
                              "2023T2.Homem Branco", "2024T2.Homem Branco",
                              "2023T2.Mulher Branca", "2024T2.Mulher Branca",
                              "2023T2.Homem Negro", "2024T2.Homem Negro",
                              "2023T2.Mulher Negra", "2024T2.Mulher Negra"),
                   labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = "2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024",
                              "2023T2.Brasil" = "2023",
                              "2024T2.Brasil" = "2024",
                              "2023T2.Invisivel" = "",
                              "2024T2.Invisivel" = "")) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 4)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 26),
        legend.title = element_text(size = 34),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 34, margin = margin(b = 20)), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, margin = margin(t = 10, b = 10)), 
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  labs(x = "", y = "%", title = "Taxa de Desemprego",
        subtitle = "2º trimestre, 25-65 anos",
       caption = "Fonte: PNAD Contínua, IBGE")
ggsave(file.path(figures_output, "desemprego_atual.png"), plot = unemp, 
       width = 12, height = 8, dpi = 500)

# pea
pea <- dt1 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), 
             y = 100*pea_fac, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(100*pea_fac)), vjust = -0.5, 
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "", 
                    values = color_squeme,
                    breaks = breaks_seq,
                    labels = labels_seq,
                    drop = FALSE) + 
  scale_x_discrete(limits = c("2023T2.Brasil", "2024T2.Brasil",
                              "2023T2.Homem Branco", "2024T2.Homem Branco",
                              "2023T2.Mulher Branca", "2024T2.Mulher Branca",
                              "2023T2.Homem Negro", "2024T2.Homem Negro",
                              "2023T2.Mulher Negra", "2024T2.Mulher Negra"),
                   labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = "2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024",
                              "2023T2.Brasil" = "2023",
                              "2024T2.Brasil" = "2024",
                              "2023T2.Invisivel" = "",
                              "2024T2.Invisivel" = "")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 24),
        legend.position = "bottom",
        legend.title = element_text(size = 34),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 34, margin = margin(b = 20)), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, margin = margin(t = 10, b = 10)), 
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  labs(x = "", y = "%", title = "População Economicamente Ativa",
       subtitle = "2º trimestre, 25-65 anos",
       caption = "Fonte: PNAD Contínua, IBGE")
ggsave(file.path(figures_output, "pea_atual.png"), plot = pea, 
       width = 12, height = 8, dpi = 500)

# gph 4 - massa salarial habitual
dt2 <- dt1 %>%  
  filter(gender_race != "Brasil" & gender_race != "Invisivel") 
dt2[, massa_hab := massa_hab/1000000000]

massa_hab <- dt2 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), 
             y = massa_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(massa_hab)), vjust = -0.5, 
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = " 2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024")) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 26),
        legend.position = "bottom",
        legend.title = element_text(size = 34),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 34, margin = margin(b = 20)), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, margin = margin(t = 10, b = 10)),  
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
  labs(x = "", y = "R$ em bilhões", title = "Massa Salarial",
       subtitle = "2º trimestre, 25-65 anos",
       caption = "Fonte: PNAD Contínua, IBGE")
ggsave(file.path(figures_output, "massa_atual.png"), plot = massa_hab, 
       width = 12, height = 8, dpi = 500)



