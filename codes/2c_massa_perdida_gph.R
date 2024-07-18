# Criando gph da massa salarial perdida 

a <- fread(file.path(csv_files, "resultados_massa_salarial.csv"))


# Massa Total
pdf(file.path(figures_output, "massa_salarial_total_genero_raca.pdf"),  width = 14, height = 8.5)

m <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_total_perdida_hn, 
                color = "Homens Negros"), group = 1, size = 1.8) +
  geom_line(aes(y = massa_salarial_total_perdida_mn, 
                color = "Mulheres Negras"), group = 1, size = 1.8) +
  scale_color_manual(name = "", values = c("Homens Negros" = "darkgoldenrod1",
                                           "Mulheres Negras" = "brown4")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text( vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  labs(x = "", y = "R$ bilhões",title = "")

print(m)
dev.off()

# Gph perda salarial devido a caracteristicas dos trabalhadores e a discriminacao
perda <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_perdida_hn*(-1), 
                color = "Homens Negros"), group = 1, size = 1.8) +
  geom_line(aes(y = massa_salarial_perdida_mn*(-1), 
                color = "Mulheres Negras"), group = 1, size = 1.8) +
  scale_color_manual(name = "", values = c("Homens Negros" = "darkgoldenrod1",
                                           "Mulheres Negras" = "brown4")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text( vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  labs(x = "", y = "R$ bilhões",title = "")


pdf(file.path(figures_output, "perda_massa_salarial.pdf"),  width = 14, height = 8.5)
print(perda)
dev.off()

# Gph perda salarial devido a caracteristicas dos trabalhadores
pdf(file.path(figures_output, "perda_massa_salarial_composicao.pdf"),  width = 14, height = 8.5)

perda_wg <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_composicao_hn*(-1), 
                color = "Homens Negros"),
            group = 1, size = 1.8) +
  geom_line(aes(y = massa_salarial_composicao_mn*(-1), 
                color = "Mulheres Negras"),
            group = 1, size = 1.8) +
  scale_color_manual(name = "", values = c("Homens Negros"
                                           = "darkgoldenrod1",
                                           "Mulheres Negras"
                                           = "brown4")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  labs(x = "", y = "R$ Bilhões",title = "")

print(perda_wg)
dev.off()

# Gph perda salarial devido ao componente discriminatorio
pdf(file.path(figures_output, "perda_massa_salario_discriminacao.pdf"),  width = 14, height = 8.5)

perda_disr_wg <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_discriminacao_hn*(-1), 
                color = "Homens Negros"),
            group = 1, size = 1.8) +
  geom_line(aes(y = massa_salarial_discriminacao_mn*(-1), 
                color = "Mulheres Negras"),
            group = 1, size = 1.8) +
  scale_color_manual(name = "", values = c("Homens Negros" = "darkgoldenrod1",
                                           "Mulheres Negras" = "brown4")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  labs(x = "", y = "R$ Bilhões",title = "")

print(perda_disr_wg)
dev.off()

# Gph perda na empregabilidade devido a caracteristicas dos trabalhadores e a discriminacao
pdf(file.path(figures_output, "perda_massa_emp_total.pdf"),  width = 14, height = 8.5)

perda_emp_total <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_perdida_emp_hn*(-1), 
                color = "Homens Negros"),
            group = 1, size = 1.8) +
  geom_line(aes(y = massa_salarial_perdida_emp_mn*(-1), 
                color = "Mulheres Negras"),
            group = 1, size = 1.8) +
  scale_color_manual(name = "", values = c("Homens Negros" = "darkgoldenrod1",
                                           "Mulheres Negras"  = "brown4")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text( vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5))  +
  labs(x = "", y = "R$ Bilhões",title = "")

print(perda_emp_total)
dev.off()


# Gph perda na empregabilidade devido a caracteristicas dos trabalhadores
pdf(file.path(figures_output, "perda_massa_emp_composicao.pdf"),  width = 14, height = 8.5)

perda_emp <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_composicao_emp_hn*(-1), 
                color = "Homens Negros"),
            group = 1, size = 1.8) +
  geom_line(aes(y = massa_salarial_composicao_emp_mn*(-1), 
                color = "Mulheres Negras"),
            group = 1, size = 1.8) +
  scale_color_manual(name = "", values = c("Homens Negros" = "darkgoldenrod1",
                                           "Mulheres Negras"  = "brown4")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text( vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5))  +
  labs(x = "", y = "R$ Bilhões",title = "")

print(perda_emp)
dev.off()



# Gph perda na empregabilidade devido ao componente discriminatorio
pdf(file.path(figures_output, "perda_massa_emp_discriminacao.pdf"),  width = 14, height = 8.5)

perda_disr_emp <- a %>% 
  ggplot(aes(x = Ano_trimestre)) + 
  geom_line(aes(y = massa_salarial_discriminacao_emp_hn, 
                color = "Homens Negros"),
            group = 1, size = 1.8) +
  geom_line(aes(y = massa_salarial_discriminacao_emp_mn, 
                color = "Mulheres Negras"),
            group = 1, size = 1.8) +
  scale_color_manual(name = "", values = c("Homens Negros" = "darkgoldenrod1",
                                           "Mulheres Negras" = "brown4")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text( vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5))  +
  labs(x = "", y = "R$ Bilhões",title = "")

print(perda_disr_emp)
dev.off()
