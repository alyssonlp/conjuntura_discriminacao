# Criando gph da massa salarial perdida 

a <- fread(file.path(csv_files, "massa_salarial_perdida_genero_raca.csv"))

pdf(file.path(figures_output, "massa_salarial_perdida_genero_raca.pdf"),  width = 12, height = 8.5)

m <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_perdida_homem_negro.V1/1000000000, 
                color = "Massa salarial perdida \n dos homens negros"), group = 1, size = 1.2) +
  geom_line(aes(y = massa_salarial_perdida_mulher_negra.V1/1000000000, 
                color = "Massa salarial perdida \n das mulheres negras"), group = 1, size = 1.2) +
  scale_color_manual(name = "", values = c("Massa salarial perdida \n dos homens negros" = "darkgoldenrod1",
                                           "Massa salarial perdida \n das mulheres negras" = "brown4")) +
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
  labs(x = "", y = "Em R$ bilhões",title = "")

print(m)
dev.off()

# Gph perda salarial devido a caracteristicas dos trabalhadores
pdf(file.path(figures_output, "perda_salarial_genero_raca.pdf"),  width = 12, height = 8.5)

perda_wg <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = diff_caracteristica_wg_homem_negro.V1, 
                color = "Perda salarial devido às características \n dos trabalhadores negros"),
            group = 1, size = 1.2) +
  geom_line(aes(y = diff_caracteristica_wg_mulher_negra.V1, 
                color = "Perda salarial devido  às características \n das trabalhadoras negras"),
            group = 1, size = 1.2) +
  scale_color_manual(name = "", values = c("Perda salarial devido às características \n dos trabalhadores negros"
                                           = "darkgoldenrod1",
                                           "Perda salarial devido  às características \n das trabalhadoras negras"
                                           = "brown4")) +
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
  labs(x = "", y = "Escala logarítmica",title = "")

print(perda_wg)
dev.off()

# Gph perda na empregabilidade devido a caracteristicas dos trabalhadores
pdf(file.path(figures_output, "perda_emp_genero_raca.pdf"),  width = 12, height = 8.5)

perda_emp <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = diff_caracteristica_emp_homem_negro.V1, 
                color = "Perda da empregabilidade devido às \n características dos trabalhadores negros"),
            group = 1, size = 1.2) +
  geom_line(aes(y = diff_caracteristica_emp_mulher_negra.V1, 
                color = "Perda da empregabilidade devido às \n características das trabalhadoras negras"),
            group = 1, size = 1.2) +
  scale_color_manual(name = "", values = c("Perda da empregabilidade devido às \n características dos trabalhadores negros" = "darkgoldenrod1",
                                           "Perda da empregabilidade devido às \n características das trabalhadoras negras"  = "brown4")) +
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
  labs(x = "", y = "Escala logarítmica",title = "")

print(perda_emp)
dev.off()

# Gph perda salarial devido ao componente discriminatorio
pdf(file.path(figures_output, "perda_salarial_discr_genero_raca.pdf"),  width = 12, height = 8.5)

perda_disr_wg <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = perda_wg_discr_homem_negro.V1, 
                color = "Perda salarial dos trabalhadores negros \n devido à discriminação racial"),
            group = 1, size = 1.2) +
  geom_line(aes(y = perda_wg_discr_mulher_negra.V1, 
                color = "Perda salarial das trabalhadoras negras \n devido à discriminação racial"),
            group = 1, size = 1.2) +
  scale_color_manual(name = "", values = c("Perda salarial dos trabalhadores negros \n devido à discriminação racial" = "darkgoldenrod1",
                                           "Perda salarial das trabalhadoras negras \n devido à discriminação racial" = "brown4")) +
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
  labs(x = "", y = "Escala logarítmica",title = "")

print(perda_disr_wg)
dev.off()

# Gph perda na empregabilidade devido ao componente discriminatorio
pdf(file.path(figures_output, "perda_emp_discr_genero_raca.pdf"),  width = 12, height = 8.5)

perda_disr_emp <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = perda_emp_discr_homem_negro.V1, 
                color = "Perda da empregabilidade dos trabalhadores \n negros devido à discriminação racial"),
            group = 1, size = 1.2) +
  geom_line(aes(y = perda_emp_discr_mulher_negra.V1, 
                color = "Perda da empregabilidade das trabalhadoras \n negras devido à discriminação racial"),
            group = 1, size = 1.2) +
  scale_color_manual(name = "", values = c("Perda da empregabilidade dos trabalhadores \n negros devido à discriminação racial" = "darkgoldenrod1",
                                           "Perda da empregabilidade das trabalhadoras \n negras devido à discriminação racial" = "brown4")) +
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
  labs(x = "", y = "Escala logarítmica",title = "")

print(perda_disr_emp)
dev.off()
