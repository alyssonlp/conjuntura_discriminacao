gen_raca <- fread(file.path(csv_files, "resultados_genero_raca_carta.csv"))
gen_raca <- gen_raca[, gender_race := gsub("_", " ", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("homem", "Homem", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("mulher", "Mulher", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negro", "Negro", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negra", "Negra", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branco", "Branco", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branca", "Branca", gender_race)]
# gph 1 - rendimento habitual médio


pdf(file.path(figures_output, "rendimento_habitual_gen_raca.pdf"),  width = 12, height = 8.5)
gen_raca_r_hab_all <- gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = renda_media_hab, color = gender_race, group = gender_race)) + 
  geom_line(size = 1) +
  scale_color_manual(name = "", values = c("Homem Branco" = "darkslategray",
                                           "Mulher Branca" = "darkorange1",
                                           "Homem Negro" = "blue3",
                                           "Mulher Negra" = "brown2")) +
  scale_x_discrete(breaks = c("2012T1", "2012T3", "2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 18),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))  +
  labs(x = "", y = "(R$)",
       title = "Rendimento Habitual Médio por Gênero e Raça - Todos os Trabalhos")

print(gen_raca_r_hab_all)
dev.off()

# gph 2 - rendimento habitual e efetivo medio mensal (dessazonalizado)
# baixar os dados mensais e dessazonalizar

pdf(file.path(figures_output, "rendimento_habitual_efetivo_gen_raca.pdf"),  width = 12, height = 8.5)
gen_raca_r_hab_efe_all <- gen_raca %>% 
  ggplot( aes(x = Ano_trimestre, color = gender_race, group = gender_race)) + 
  geom_line(aes(y = renda_media_hab, linetype = "Renda Habitual Média"), size = 1) +
  geom_line(aes(y = renda_media_efe, linetype = "Renda Efetiva Média"), size = 1) +
  scale_linetype_manual(name = "",
                        values = c("Renda Habitual Média" ="solid", 
                                   "Renda Efetiva Média" = "dashed")) +
  scale_color_manual(name = "", values = c("Homem Branco" = "darkslategray",
                                           "Mulher Branca" = "darkorange1",
                                           "Homem Negro" = "blue3",
                                           "Mulher Negra" = "brown2")) +
  scale_x_discrete(breaks = c("2012T1", "2012T3", "2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "(R$)", 
       title = "Rendimento Habitual e Efetivo Médio por Gênero e Raça",
       linetype = "Variável", color = "Raça")

print(gen_raca_r_hab_efe_all)
dev.off()

# gph 4 - massa salarial real habitual e efetiva
gen_raca <- gen_raca[, massa_hab := massa_hab/1000000]
gen_raca <- gen_raca[, massa_efe := massa_efe/1000000]

pdf(file.path(figures_output, "massa_habitual_efetivo_gen_raca.pdf"),  width = 12, height = 8.5)
gen_raca_massa_hab_efe <- gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, color = gender_race, group = gender_race)) + 
  geom_line(aes(y = massa_hab, linetype = "Massa Salarial Habitual"), size = 1) +
  geom_line(aes(y = massa_efe, linetype = "Massa Salarial Efetiva"), size = 1) +
  scale_linetype_manual(name = "", values = c("Massa Salarial Habitual" ="solid", 
                                              "Massa Salarial Efetiva" = "dashed")) +
  scale_color_manual(name = "", values = c("Homem Branco" = "darkslategray",
                                           "Mulher Branca" = "darkorange1",
                                           "Homem Negro" = "blue3",
                                           "Mulher Negra" = "brown2")) +
  scale_x_discrete(breaks = c("2012T1", "2012T3", "2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))  +
  labs(x = "", y = "(Em R$ milhões)",
       title = "Massa Salarial Habitual e Efetiva por Gênero e Raça")

print(gen_raca_massa_hab_efe)
dev.off()

# gph 5 - taxa de variacao interanual da massa salarial real habitual e efetiva
gen_raca <- setorder(gen_raca, gender_race)
gen_raca <- gen_raca[, massa_hab_inter := (((massa_hab /lag(massa_hab, 4)) - 1) * 100), by = gender_race]
gen_raca <- gen_raca[, massa_efe_inter := (((massa_efe /lag(massa_efe, 4)) - 1) * 100), by = gender_race]
gen_raca_inter <- gen_raca[!is.na(massa_hab_inter)]

pdf(file.path(figures_output, "massa_habitual_efetivo_interanual_raca.pdf"),  width = 12, height = 8.5)
gen_raca_massa_hab_efe_inter <- gen_raca_inter %>% 
  ggplot(aes(x = Ano_trimestre, color = gender_race, group = gender_race)) + 
  geom_line(aes(y = massa_hab_inter, linetype = "Variação (%) Massa Salarial Habitual"), size = 1) +
  geom_line(aes(y = massa_efe_inter, linetype = "Variação (%) Massa Salarial Efetiva"), size = 1) +
  scale_linetype_manual(name = "", values = c("Variação (%) Massa Salarial Habitual" = "solid", 
                                              "Variação (%) Massa Salarial Efetiva" = "dashed")) +
  scale_color_manual(name = "", values = c("Homem Branco" = "darkslategray",
                                           "Mulher Branca" = "darkorange1",
                                           "Homem Negro" = "blue3",
                                           "Mulher Negra" = "brown2")) +
  scale_x_discrete(breaks = c("2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 20),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))  +
  labs(x = "", y = "(%)",
       title = " Taxa de Variação Interanual da Massa Salarial Real Habitual e Efetiva por Gênero e Raça")

print(gen_raca_massa_hab_efe_inter)
dev.off()


# gph 6 - índice de gini - rendimento habitual de todos os trabalhos - renda individual e domicilIar
pdf(file.path(figures_output, "gini_gen_raca.pdf"),  width = 12, height = 8.5)
gini_gen_raca <- gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = gini_hab, color = gender_race, group = gender_race)) + 
  geom_line(size = 1) +
  scale_color_manual(name = "", values = c("Homem Branco" = "darkslategray",
                                           "Mulher Branca" = "darkorange1",
                                           "Homem Negro" = "blue3",
                                           "Mulher Negra" = "brown2")) +
  scale_x_discrete(breaks = c("2012T1", "2012T3", "2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))  +
  labs(x = "", y = "Índice de Gini",
       title = "Índice de Gini: indicador de desigualdade do rendimento habitual
       individual de todos os trabalhos por gênero raça")

print(gini_gen_raca)
dev.off()