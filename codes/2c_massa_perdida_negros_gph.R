massa <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

# Criando totais
massa <- massa[, total_perdido := total_perdido_hn + total_perdido_mn] 
massa <- massa[, wg_composicao := massa_wg_composicao_hn + massa_wg_composicao_mn]
massa <- massa[, wg_discriminacao := massa_wg_discriminacao_hn + massa_wg_discriminacao_mn]
massa <- massa[, emp_composicao := massa_emp_composicao_hn + massa_emp_composicao_mn]
massa <- massa[, emp_discriminacao := massa_emp_discriminacao_hn + massa_emp_discriminacao_mn]

massa <- massa[, .(Ano_trimestre, total_perdido, wg_composicao, wg_discriminacao,
                   emp_composicao, emp_discriminacao)]

massa <- setnames(massa, c("total_perdido", "wg_composicao", "wg_discriminacao",
                           "emp_composicao", "emp_discriminacao"),
                  c("Massa Total Perdida", "Efeito Composição - Salários",
                    "Efeito Discriminação - Salários", "Efeito Composição - Empregabilidade",
                    "Efeito Discriminação - Empregabilidade"))


t_long <- melt(massa, id.vars = "Ano_trimestre", 
               variable.name = "Decomposição", 
               value.name = "Perda")


pdf(file.path(figures_output, "massa_perdida_gph.pdf"),  width = 14, height = 8.5)
t_results <-  t_long %>%  ggplot() + 
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
print(t_results)
dev.off()