b <- a[, penalidade_salarial_hn:= diff_caracteristica_wg_homem_negro.V1 + perda_wg_discr_homem_negro.V1]
b <- a[, penalidade_salarial_mn:= diff_caracteristica_wg_mulher_negra.V1 + perda_wg_discr_mulher_negra.V1]
b <- a[, penalidade_empregabilidade_hn:= diff_caracteristica_emp_homem_negro.V1 + perda_emp_discr_homem_negro.V1]
b <- a[, penalidade_empregabilidade_mn:= diff_caracteristica_emp_mulher_negra.V1 + perda_emp_discr_mulher_negra.V1]

# Gph penalidade salarial
pdf(file.path(figures_output, "penalidade_wg_genero_raca.pdf"),  width = 12, height = 8.5)

penal_wg <- b %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = penalidade_salarial_hn, 
                color = "Penalidade salarial dos trabalhadores negros"),
            group = 1, size = 1.2) +
  geom_line(aes(y = penalidade_salarial_mn, 
                color = "Penalidade salarial das trabalhadoras negras"),
            group = 1, size = 1.2) +
  scale_color_manual(name = "", values = c("Penalidade salarial dos trabalhadores negros" = "darkgoldenrod1",
                                           "Penalidade salarial das trabalhadoras negras" = "brown4")) +
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
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=18))  +
  labs(x = "", y = "Ln rendimento habitual",title = "")

print(penal_wg)
dev.off()

# Gph penalidade na empregabilidade
pdf(file.path(figures_output, "penalidade_emp_genero_raca.pdf"),  width = 12, height = 8.5)

penal_emp <- b %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = penalidade_empregabilidade_hn, 
                color = "Penalidade na empregabilidade dos trabalhadores negros"),
            group = 1, size = 1.2) +
  geom_line(aes(y = penalidade_empregabilidade_mn, 
                color = "Penalidade na empregabilidade das trabalhadoras negras"),
            group = 1, size = 1.2) +
  scale_color_manual(name = "", values = c("Penalidade na empregabilidade dos trabalhadores negros" = "darkgoldenrod1",
                                           "Penalidade na empregabilidade das trabalhadoras negras" = "brown4")) +
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
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=16))  +
  labs(x = "", y = "",title = "")

print(penal_emp)
dev.off()