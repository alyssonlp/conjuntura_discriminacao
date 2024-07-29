massa_homens <- fread(file.path(csv_output, "resultados_massa_salarial_homens.csv"))
massa_mulheres <- fread(file.path(csv_output, "resultados_massa_salarial_mulheres.csv"))

massa <- cbind(massa_homens, massa_mulheres)

# Criando totais: homem branco + mulher branca
massa[, total_b := total_hb + total_mb]
massa[, composicao_wg_b := composicao_massa_wg_hb + composicao_massa_wg_mb]
massa[, composicao_emp_b := composicao_massa_emp_hb + composicao_massa_emp_mb ]

brancos <- massa[, .(Ano_trimestre, total_b, composicao_wg_b, composicao_emp_b)]

setnames(brancos, c("total_b", "composicao_wg_b", "composicao_emp_b" ),
         c("Massa Salarial Premiada",
           "Efeito Composição - Salários",
            "Efeito Composição - Empregabilidade"))


b_long <- melt(brancos, id.vars = "Ano_trimestre", 
               variable.name = "Decomposição", 
               value.name = "Perda")

# Filtrando as variaveis que serão usadas para o estilo de area
b_long_area <- b_long %>% filter(Decomposição != "Massa Salarial Premiada")

# Filtrando a variavel uqe será usada para o estilo de linha
b_long_linha <-  b_long %>% filter(Decomposição == "Massa Salarial Premiada")


pdf(file.path(figures_output, "massa_premiada_brancos_gph.pdf"),  width = 14, height = 8.5)
b_results <-   ggplot() + 
  geom_area(data = b_long_area, aes(x = Ano_trimestre, y = Perda, group = Decomposição, fill = Decomposição)) +
  geom_line(data = b_long_linha, aes(x = Ano_trimestre, y = Perda, group = 1, color = Decomposição), size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_manual(values = c("Massa Salarial Premiada" = "black")) +
  scale_x_discrete(breaks = c("2012T1", "2016T1","2020T1", "2024T1"),
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

print(b_results)
dev.off()


# Para os negros
# Criando totais: homem negro + mulher negra
massa[, total_n := total_hn + total_mn]
massa[, discriminacao_wg_n := discriminacao_massa_wg_hn + discriminacao_massa_wg_mn]
massa[, composicao_wg_n := composicao_massa_wg_hn + composicao_massa_wg_mn]
massa[, discriminacao_emp_n := discriminacao_massa_emp_hn + discriminacao_massa_emp_mn]
massa[, composicao_emp_n := composicao_massa_emp_hn + composicao_massa_emp_mn]

negros <- massa[, .(Ano_trimestre, total_n, discriminacao_wg_n, composicao_wg_n,
                    discriminacao_emp_n, composicao_emp_n )]

setnames(negros, c("total_n", "discriminacao_wg_n", "composicao_wg_n",
                    "discriminacao_emp_n", "composicao_emp_n"),
         c("Massa Salarial Perdida", "Efeito Discriminação - Salários",
           "Efeito Composição - Salários", "Efeito Discriminação - Empregabilidade",
           "Efeito Composição - Empregabilidade"))


n_long <- melt(negros, id.vars = "Ano_trimestre", 
               variable.name = "Decomposição", 
               value.name = "Perda")

# Filtrando as variaveis que serão usadas para o estilo de area
n_long_area <- n_long %>% filter(Decomposição != "Massa Salarial Perdida")

# Filtrando a variavel uqe será usada para o estilo de linha
n_long_linha <-  n_long %>% filter(Decomposição == "Massa Salarial Perdida")

pdf(file.path(figures_output, "massa_perdida_negros_gph.pdf"),  width = 14, height = 8.5)
n_results <-   ggplot() + 
  geom_area(data = n_long_area, aes(x = Ano_trimestre, y = Perda*(-1), group = Decomposição, fill = Decomposição)) +
  geom_line(data = n_long_linha, aes(x = Ano_trimestre, y = Perda*(-1), group = 1, color = Decomposição), size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_brewer(palette = "PuOr") + 
  scale_color_manual(values = c("Massa Salarial Perdida" = "black")) +
  scale_x_discrete(breaks = c("2012T1", "2016T1","2020T1", "2024T1"),
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

print(n_results)
dev.off()