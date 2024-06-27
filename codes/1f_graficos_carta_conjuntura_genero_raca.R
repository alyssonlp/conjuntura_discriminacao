gen_raca <- fread(file.path(csv_files, "resultados_genero_raca_carta.csv"))
gen_raca <- gen_raca[, gender_race := gsub("_", " ", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("homem", "Homem", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("mulher", "Mulher", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negro", "Negro", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negra", "Negra", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branco", "Branco", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branca", "Branca", gender_race)]

brasil <- fread(file.path(csv_files, "resultados_brasil_carta.csv"))

br_gen_raca <- rbind(gen_raca, brasil)

# gph 1 - rendimento habitual médio
pdf(file.path(figures_output, "rendimento_habitual_br_gen_raca.pdf"),  width = 12, height = 8.5)
br_gen_raca_r_hab_all <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = renda_media_hab, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "", values = c("Homem Branco" ="solid",
                                   "Mulher Branca" =  "twodash", 
                                   "Homem Negro" = "solid", 
                                   "Mulher Negra" ="twodash",
                                   "Brasil" = "solid")) +
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

print(br_gen_raca_r_hab_all)
dev.off()

# gph 2 - rendimento efetivo 

pdf(file.path(figures_output, "rendimento_efetivo_br_gen_raca.pdf"),  width = 12, height = 8.5)
br_gen_raca_r_efe_all <- br_gen_raca %>% 
  ggplot( aes(x = Ano_trimestre, y = renda_media_efe, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
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
       title = "Rendimento Efetivo Médio por Gênero e Raça")

print(br_gen_raca_r_efe_all)
dev.off()

# gph 4a - massa salarial real habitual 
br_gen_raca <- br_gen_raca[, massa_hab := massa_hab/1000000000]
br_gen_raca <- br_gen_raca[, massa_efe := massa_efe/1000000000]

pdf(file.path(figures_output, "massa_habitual_br_gen_raca.pdf"),  width = 12, height = 8.5)
br_gen_raca_massa_hab <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = massa_hab, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
  scale_x_discrete(breaks = c("2012T1", "2012T3", "2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))  +
  labs(x = "", y = "(Em R$ bilhões)",
       title = "Massa Salarial Habitual por Gênero e Raça")

print(br_gen_raca_massa_hab)
dev.off()

# gph 4b - massa salarial real efetiva
pdf(file.path(figures_output, "massa_efetivo_br_gen_raca.pdf"),  width = 12, height = 8.5)
br_gen_raca_massa_efe <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y= massa_efe, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
  scale_x_discrete(breaks = c("2012T1", "2012T3", "2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))  +
  labs(x = "", y = "(Em R$ bilhões)",
       title = "Massa Salarial Efetiva por Gênero e Raça")

print(br_gen_raca_massa_efe)
dev.off()

# gph 5a - taxa de variacao interanual da massa salarial real habitual 
br_gen_raca <- setorder(br_gen_raca, gender_race)
br_gen_raca <- br_gen_raca[, massa_hab_inter := (((massa_hab /lag(massa_hab, 4)) - 1) * 100), by = gender_race]
br_gen_raca <- br_gen_raca[, massa_efe_inter := (((massa_efe /lag(massa_efe, 4)) - 1) * 100), by = gender_race]
br_gen_raca_inter <- br_gen_raca[!is.na(massa_hab_inter)]

pdf(file.path(figures_output, "massa_habitual_interanual_br_gen_raca.pdf"),  width = 12, height = 8.5)
br_gen_raca_massa_hab_inter <- br_gen_raca_inter %>% 
  ggplot(aes(x = Ano_trimestre, y = massa_hab_inter, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
  scale_x_discrete(breaks = c("2013T1", "2013T3",
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
  labs(x = "", y = "(%)",
       title = " Taxa de Variação Interanual da Massa Salarial Real Habitual por Gênero e Raça")

print(br_gen_raca_massa_hab_inter)
dev.off()

# gph 5b - taxa de variacao interanual da massa salarial real efetiva

pdf(file.path(figures_output, "massa_efetivo_interanual_br_gen_raca.pdf"),  width = 12, height = 8.5)
br_gen_raca_massa_efe_inter <- br_gen_raca_inter %>% 
  ggplot(aes(x = Ano_trimestre, y = massa_efe_inter, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
  scale_x_discrete(breaks = c("2013T1", "2013T3",
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
  labs(x = "", y = "(%)",
       title = " Taxa de Variação Interanual da Massa Salarial Real Efetiva por Gênero e Raça")

print(br_gen_raca_massa_efe_inter)
dev.off()


# gph 6 - índice de gini - rendimento habitual de todos os trabalhos - renda individual e domicilIar
pdf(file.path(figures_output, "gini_br_gen_raca.pdf"),  width = 12, height = 8.5)
gini_br_gen_raca <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = gini_hab, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
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

print(gini_br_gen_raca)
dev.off()

# gph unemp
pdf(file.path(figures_output, "unemp_br_gen_raca.pdf"),  width = 12, height = 8.5)
unemp_br_gen_raca <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = tx_desocup*100, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
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
  labs(x = "", y =  "(%)",
       title = "Taxa de Desocupação por Gênero e Raça")

print(unemp_br_gen_raca)
dev.off()

# gph PEA
pdf(file.path(figures_output, "pea_br_gen_raca.pdf"),  width = 12, height = 8.5)
pea_br_gen_raca <- br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = pea_fac*100, color = gender_race, group = gender_race)) + 
  geom_line(size = 1.4, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "darkorange1",
                                "Mulher Branca" = "darkorange1",
                                "Homem Negro" = "blue3",
                                "Mulher Negra" = "blue3",
                                "Brasil" = "black"))+
  scale_linetype_manual(name = "",values = c("Homem Branco" ="solid",
                                             "Mulher Branca" =  "twodash", 
                                             "Homem Negro" = "solid", 
                                             "Mulher Negra" ="twodash",
                                             "Brasil" = "solid")) +
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
  labs(x = "", y =  "(%)",
       title = "População Economicamente Ativa por Gênero e Raça")

print(pea_br_gen_raca)
dev.off()


# salvando
write.csv(br_gen_raca, file.path(csv_files, "descriptive_br_gender_race_carta.csv"))