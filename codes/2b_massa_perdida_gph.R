# Criando gph da massa salarial perdida 

mass <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

# Gph para homens negros
# total, composicao wg, discriminacao wg, composicao emp, discriminacao emp
# Transformar os dados para o formato longo
homem <- mass[, .(Ano_trimestre, massa_wg_composicao_hn, massa_wg_discriminacao_hn,
                  massa_emp_composicao_hn, massa_emp_discriminacao_hn)]
homem[, massa_perdida := massa_wg_composicao_hn + massa_wg_discriminacao_hn +
        massa_emp_composicao_hn + massa_emp_discriminacao_hn]

setnames(homem, c( "massa_wg_composicao_hn", "massa_wg_discriminacao_hn",
                           "massa_emp_composicao_hn", "massa_emp_discriminacao_hn", "massa_perdida"),
                  c("Efeito Composição - Salários",
                    "Efeito Discriminação - Salários", "Efeito Composição - Empregabilidade",
                    "Efeito Discriminação - Empregabilidade", "Massa Salarial Perdida"))

h_long <- melt(homem, id.vars = "Ano_trimestre", 
                variable.name = "Decomposição", 
                value.name = "Perda")

# Filtrando as variaveis que serão usadas para o estilo de area
h_long_area <- h_long %>% filter(Decomposição != "Massa Salarial Perdida")

# Filtrando a variavel uqe será usada para o estilo de linha
h_long_linha <-  h_long %>% filter(Decomposição == "Massa Salarial Perdida")


pdf(file.path(figures_output, "homem_negro_massa_perdida_gph.pdf"),  width = 14, height = 8.5)
hn_results <- ggplot() + 
  geom_area(data = h_long_area, aes(x = Ano_trimestre, y = Perda*(-1), group = Decomposição, fill = Decomposição)) +
  geom_line(data = h_long_linha, aes(x = Ano_trimestre, y = Perda*(-1), group = 1, color = Decomposição), size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_brewer(palette = "PuOr") + 
  scale_color_manual(values = c("Massa Salarial Perdida" = "black")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1", "2022T1", "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y = "R$ bilhões", title = "")

print(hn_results)
dev.off()


# Para mulheres negras
mulher <- mass[, .(Ano_trimestre, massa_wg_composicao_mn, massa_wg_discriminacao_mn,
                  massa_emp_composicao_mn, massa_emp_discriminacao_mn)]
mulher[, massa_perdida := massa_wg_composicao_mn + massa_wg_discriminacao_mn +
        massa_emp_composicao_mn + massa_emp_discriminacao_mn]

setnames(mulher, c( "massa_wg_composicao_mn", "massa_wg_discriminacao_mn",
                           "massa_emp_composicao_mn", "massa_emp_discriminacao_mn", "massa_perdida"),
                  c( "Efeito Composição - Salários",
                    "Efeito Discriminação - Salários", "Efeito Composição - Empregabilidade",
                    "Efeito Discriminação - Empregabilidade", "Massa Salarial Perdida" ))

m_long <- melt(mulher, id.vars = "Ano_trimestre", 
               variable.name = "Decomposição", 
               value.name = "Perda")

# Filtrando as variaveis que serão usadas para o estilo de area
m_long_area <- m_long %>% filter(Decomposição != "Massa Salarial Perdida")

# Filtrando a variavel uqe será usada para o estilo de linha
m_long_linha <-  m_long %>% filter(Decomposição == "Massa Salarial Perdida")


pdf(file.path(figures_output, "mulher_negra_massa_perdida_gph.pdf"),  width = 14, height = 8.5)
mn_results <-ggplot() + 
  geom_area(data = m_long_area, aes(x = Ano_trimestre, y = Perda*(-1), group = Decomposição, fill = Decomposição)) +
  geom_line(data = m_long_linha, aes(x = Ano_trimestre, y = Perda*(-1), group = 1, color = Decomposição), size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_brewer(palette = "PuOr") + 
  scale_color_manual(values = c("Massa Salarial Perdida" = "black")) +
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1", "2022T1", "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y = "R$ bilhões", title = "")

print(mn_results)
dev.off()