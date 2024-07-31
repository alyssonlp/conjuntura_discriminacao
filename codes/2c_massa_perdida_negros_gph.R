massa <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

# Criando totais: homem + mulher
massa[, wg_composicao := composicao_massa_wg_hn + composicao_massa_wg_mn]
massa[, wg_discriminacao := discriminacao_massa_wg_hn + discriminacao_massa_wg_mn]
massa[, emp_composicao := composicao_massa_emp_hn + composicao_massa_emp_mn]
massa[, emp_discriminacao := discriminacao_massa_emp_hn + discriminacao_massa_emp_mn]
massa[, massa_perdida := total_hn + total_mn]

massa <- massa[, .(Ano_trimestre, wg_composicao, wg_discriminacao,
                   emp_composicao, emp_discriminacao, massa_perdida)]

setnames(massa, c( "wg_composicao", "wg_discriminacao",
                           "emp_composicao", "emp_discriminacao", "massa_perdida"),
                  c( "Efeito Composição - Salários",
                    "Efeito Discriminação - Salários", "Efeito Composição - Empregabilidade",
                    "Efeito Discriminação - Empregabilidade", "Massa Salarial Perdida"))


t_long <- melt(massa, id.vars = "Ano_trimestre", 
               variable.name = "Decomposição", 
               value.name = "Perda")

# Filtrando as variaveis que serão usadas para o estilo de area
t_long_area <- t_long %>% filter(Decomposição != "Massa Salarial Perdida")

# Filtrando a variavel uqe será usada para o estilo de linha
t_long_linha <-  t_long %>% filter(Decomposição == "Massa Salarial Perdida")


pdf(file.path(figures_output, "massa_perdida_gph.pdf"),  width = 14, height = 8.5)
t_results <-   ggplot() + 
  geom_area(data = t_long_area, aes(x = Ano_trimestre, y = Perda*(-1), group = Decomposição, fill = Decomposição)) +
  geom_line(data = t_long_linha, aes(x = Ano_trimestre, y = Perda*(-1), group = 1, color = Decomposição), size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_brewer(palette = "PuOr") + 
  scale_color_manual(values = c("Massa Salarial Perdida" = "black")) +
  scale_x_discrete(breaks = c("2012T1", "2016T1", "2020T1", "2024T1"),
                   labels = c("2012", "2016", "2020", "2024")) +
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

print(t_results)
dev.off()