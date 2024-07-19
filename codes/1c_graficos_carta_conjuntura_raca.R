raca <- fread(file.path(csv_output, "resultados_raca_carta.csv"))

raca <- raca[nonwhite == 1, Raca := "Negros"]
raca <- raca[nonwhite == 0, Raca := "Brancos"]
raca <- raca[, nonwhite := NULL]


# gph 1 - rendimento habitual médio
raca_r_hab_all <- raca %>% 
  ggplot(aes(x = Ano_trimestre, y = renda_media_hab, color = Raca, group = Raca)) + 
  geom_line(size = 1) +
  scale_color_manual(name = "", values = c("Brancos" = "orange",
                                               "Negros" = "blue")) +
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
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22))  +
  labs(x = "", y = "R$",title = "")

pdf(file.path(figures_output, "rendimento_habitual_raca.pdf"),  width = 11, height = 8.5)
print(raca_r_hab_all)
dev.off()

# gph 2 - rendimento habitual e efetivo medio mensal 
raca_r_hab_efe_all <- ggplot(raca, aes(x = Ano_trimestre, color = Raca, group = Raca)) + 
  geom_line(aes(y = renda_media_hab, linetype = "Renda Habitual Média"), size = 1) +
  geom_line(aes(y = renda_media_efe, linetype = "Renda Efetiva Média"), size = 1) +
  scale_linetype_manual(name = "",
                        values = c("Renda Habitual Média" ="solid", 
                                   "Renda Efetiva Média" = "dashed")) +
  scale_color_manual(name = "", values = c("Brancos" = "orange",
                                               "Negros" = "blue")) +
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
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "R$", 
       title = "")
pdf(file.path(figures_output, "rendimento_habitual_efetivo_raca.pdf"),  width = 12, height = 8.5)
print(raca_r_hab_efe_all)
dev.off()

# gph 4 - massa salarial real habitual e efetiva
raca <- raca[, massa_hab := massa_hab/1000000]
raca <- raca[, massa_efe := massa_efe/1000000]

pdf(file.path(figures_output, "massa_habitual_efetivo_raca.pdf"),  width = 12, height = 8.5)
raca_massa_hab_efe <- raca %>% 
  ggplot(aes(x = Ano_trimestre, color = Raca, group = Raca)) + 
  geom_line(aes(y = massa_hab, linetype = "Massa Salarial Habitual"), size = 1) +
  geom_line(aes(y = massa_efe, linetype = "Massa Salarial Efetiva"), size = 1) +
  scale_linetype_manual(name = "", values = c("Massa Salarial Habitual" ="solid", 
                                   "Massa Salarial Efetiva" = "dashed")) +
  scale_color_manual(name = "", values = c("Brancos" = "orange",
                                               "Negros" = "blue")) +
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
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22))  +
  labs(x = "", y = "Em R$ milhões", title = "")

print(raca_massa_hab_efe)
dev.off()

# gph 5 - taxa de variacao interanual da massa salarial real habitual e efetiva
raca <- setorder(raca, Raca)
raca <- raca[, massa_hab_inter := (((massa_hab /lag(massa_hab, 4)) - 1) * 100), by = Raca]
raca <- raca[, massa_efe_inter := (((massa_efe /lag(massa_efe, 4)) - 1) * 100), by = Raca]
raca_inter <- raca[!is.na(massa_hab_inter)]

pdf(file.path(figures_output, "massa_habitual_efetivo_interanual_raca.pdf"),  width = 12, height = 8.5)
raca_massa_hab_efe_inter <- raca_inter %>% 
  ggplot(aes(x = Ano_trimestre, color = Raca, group = Raca)) + 
  geom_line(aes(y = massa_hab_inter, linetype = "Variação (%) Massa Salarial Habitual"), size = 1) +
  geom_line(aes(y = massa_efe_inter, linetype = "Variação (%) Massa Salarial Efetiva"), size = 1) +
  scale_linetype_manual(name = "", values = c("Variação (%) Massa Salarial Habitual" = "solid", 
                                              "Variação (%) Massa Salarial Efetiva" = "dashed")) +
  scale_color_manual(name = "", values = c("Brancos" = "orange",
                                           "Negros" = "blue")) +
  scale_x_discrete(breaks = c("2013T1", "2013T3",
                              "2014T1", "2014T3", "2015T1", "2015T3", 
                              "2016T1", "2016T3", "2017T1", "2017T3",  
                              "2018T1", "2018T3", "2019T1", "2019T3",
                              "2020T1", "2020T3", "2021T1", "2021T3",
                              "2022T1", "2022T3", "2023T1", "2023T3", 
                              "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=16))  +
  labs(x = "", y = "%",  title = "")

print(raca_massa_hab_efe_inter)
dev.off()


# gph 6 - índice de gini - rendimento habitual de todos os trabalhos - renda individual e domicilIar
pdf(file.path(figures_output, "gini_raca.pdf"),  width = 12, height = 8.5)
gini_raca <- raca %>% 
  ggplot(aes(x = Ano_trimestre, y = gini_hab, color = Raca, group = Raca)) + 
  geom_line (size = 1) +
  scale_color_manual(name = "", values = c("Brancos" = "orange",
                                               "Negros" = "blue")) +
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
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22))  +
  labs(x = "", y = "Índice de Gini",title = "")

print(gini_raca)
dev.off()

# gph unemp
pdf(file.path(figures_output, "unemp_raca.pdf"),  width = 12, height = 8.5)
unemp_raca <- raca %>% 
  ggplot(aes(x = Ano_trimestre, y = tx_desocup*100, color = Raca, group = Raca)) + 
  geom_line(size = 1) +
  scale_color_manual(name = "", values = c("Brancos" = "orange",
                                           "Negros" = "blue")) +
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
       title = "")

print(unemp_raca)
dev.off()

# gph PEA
pdf(file.path(figures_output, "pea_raca.pdf"),  width = 12, height = 8.5)
pea_raca <- raca %>% 
  ggplot(aes(x = Ano_trimestre, y = pea_fac*100, color = Raca, group = Raca)) + 
  geom_line(size = 1) +
  scale_color_manual(name = "", values = c("Brancos" = "orange",
                                           "Negros" = "blue")) +
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
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22))  +
  labs(x = "", y =  "%",title = "")

print(pea_raca)
dev.off()