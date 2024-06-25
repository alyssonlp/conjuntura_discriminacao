brasil <- fread(file.path(csv_files, "resultados_brasil_carta.csv"))

# gph 1 - rendimento habitual médio

pdf(file.path(figures_output, "rendimento_habitual.pdf"),  width = 11, height = 8.5)
brasil_r_hab_all <- brasil %>% 
  ggplot(aes(x = Ano_trimestre, y = renda_media_hab, group = 1)) + 
  geom_line(color = "blue", size = 1) +
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
  labs(x = "", y = "(R$)",
       title = "Rendimento Habitual Médio")

print(brasil_r_hab_all)
dev.off()

# gph 2 - rendimento habitual e efetivo medio mensal (dessazonalizado)
# baixar os dados mensais e dessazonalizar

pdf(file.path(figures_output, "rendimento_habitual_efetivo.pdf"),  width = 11, height = 8.5)
brasil_r_hab_efe_all <- brasil %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = renda_media_hab), group = 1,  color = "blue", size = 1) +
  geom_line(aes(y = renda_media_efe), group = 1,  color = "orange", size = 1) +
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
  labs(x = "", y = "(R$)",
       title = "Rendimento Habitual e Efetivo Médio")

print(brasil_r_hab_efe_all)
dev.off()

# gph 4 - massa salarial real habitual e efetiva
brasil <- brasil[, massa_hab := massa_hab/1000000]
brasil <- brasil[, massa_efe := massa_efe/1000000]

pdf(file.path(figures_output, "massa_habitual_efetivo.pdf"),  width = 11, height = 8.5)
brasil_massa_hab_efe <- brasil %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_hab), group = 1,  color = "blue", size = 1) +
  geom_line(aes(y = massa_efe), group = 1,  color = "orange", size = 1) +
  scale_linetype_manual(name = "",
                        values = c("Massa Salarial Habitual" ="solid", 
                                   "Massa Salarial Efetiva" = "dashed")) +
  scale_linetype_manual(name = "", values = c("Massa Salarial Habitual" ="solid", 
                                              "Massa Salarial Efetiva" = "dashed")) 
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
       title = "Massa Salarial Habitual e Efetiva")

print(brasil_massa_hab_efe)
dev.off()

# gph 5 - taxa de variacao interanual da massa salarial real habitual e efetiva
brasil <- brasil[, massa_hab_inter := (((massa_hab /lag(massa_hab, 4)) - 1) * 100)]
brasil <- brasil[, massa_efe_inter := (((massa_efe /lag(massa_efe, 4)) - 1) * 100)]
bra_inter <- brasil[!is.na(massa_hab_inter)]

pdf(file.path(figures_output, "massa_habitual_efetivo_interanual.pdf"),  width = 12, height = 8.5)
brasil_massa_hab_efe_inter <- bra_inter %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_hab_inter), group = 1,  color = "blue", 
            linetype = "Variação (%) Massa Salarial Habitual ", size = 1) +
  geom_line(aes(y = massa_efe_inter), group = 1,  color = "orange", 
            linetype = "Variação (%) Massa Salarial Efetiva", size = 1) +
  scale_linetype_manual(name = "",
                        values = c("Variação (%) Massa Salarial Habitual" ="solid", 
                                   "Variação (%) Massa Salarial Efetiva" = "dashed")) +
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
       title = " Taxa de Variação Interanual da Massa Salarial Real Habitual e Efetiva")

print(brasil_massa_hab_efe_inter)
dev.off()


# gph 6 - índice de gini - rendimento habitual de todos os trabalhos - renda individual e domicilIar
pdf(file.path(figures_output, "gini_brasil.pdf"),  width = 12, height = 8.5)
gini_bra <- brasil %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes (y = gini_hab), group = 1,  color = "blue", size = 1) +
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
       individual de todos os trabalhos")

print(gini_bra)
dev.off()