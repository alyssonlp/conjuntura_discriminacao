# Criando gph da massa salarial perdida 

mass <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

# Gph para homens negros
# total, composicao wg, discriminacao wg, composicao emp, discriminacao emp
# Transformar os dados para o formato longo
homem <- mass[, .(Ano_trimestre, total_perdido_hn, massa_wg_composicao_hn, massa_wg_discriminacao_hn,
                  massa_emp_composicao_hn, massa_emp_discriminacao_hn)]

homem <- setnames(homem, c("total_perdido_hn", "massa_wg_composicao_hn", "massa_wg_discriminacao_hn",
                           "massa_emp_composicao_hn", "massa_emp_discriminacao_hn"),
                  c("Massa Total Perdida", "Efeito Composição - Salários",
                    "Efeito Discriminação - Salários", "Efeito Composição - Empregabilidade",
                    "Efeito Discriminação - Empregabilidade"))

h_long <- melt(homem, id.vars = "Ano_trimestre", 
                variable.name = "Decomposição", 
                value.name = "Perda")


pdf(file.path(figures_output, "homem_negro_massa_perdida_gph.pdf"),  width = 14, height = 8.5)
hn_results <-  h_long %>%  ggplot() + 
  geom_area(aes(x = Ano_trimestre, y = Perda*(-1), group = Decomposição, fill = Decomposição)) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_brewer(palette = "PuOr") + 
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "right",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=14),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5))  +
  labs(x = "", y = "R$ bilhões", title = "")
print(hn_results)
dev.off()


# Para mulheres negras
mulher <- mass[, .(Ano_trimestre, total_perdido_mn, massa_wg_composicao_mn, massa_wg_discriminacao_mn,
                  massa_emp_composicao_mn, massa_emp_discriminacao_mn)]

mulher <- setnames(mulher, c("total_perdido_mn", "massa_wg_composicao_mn", "massa_wg_discriminacao_mn",
                           "massa_emp_composicao_mn", "massa_emp_discriminacao_mn"),
                  c("Massa Total Perdida", "Efeito Composição - Salários",
                    "Efeito Discriminação - Salários", "Efeito Composição - Empregabilidade",
                    "Efeito Discriminação - Empregabilidade"))

m_long <- melt(mulher, id.vars = "Ano_trimestre", 
               variable.name = "Decomposição", 
               value.name = "Perda")


pdf(file.path(figures_output, "mulher_negra_massa_perdida_gph.pdf"),  width = 14, height = 8.5)
mn_results <- m_long %>%  ggplot() + 
  geom_area(aes(x = Ano_trimestre, y = Perda*(-1), group = Decomposição, fill = Decomposição)) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_brewer(palette = "PuOr") + 
  scale_x_discrete(breaks = c("2012T1", "2014T1", "2016T1", "2018T1", 
                              "2020T1",  "2022T1", "2024T1")) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "right",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=14),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5))  +
  labs(x = "", y = "R$ bilhões", title = "")
print(mn_results)
dev.off()