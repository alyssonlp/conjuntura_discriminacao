dt <- fread(file.path(csv_files, "descriptive_br_gender_race_carta.csv"))

# gph 1 - renda efetiva media

pdf(file.path(figures_output, "rendimento_efetivo.pdf"),  width = 12, height = 8.5)
r_efe_all <- dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = renda_media_efe, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(renda_media_efe,2)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "Gênero e Raça",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4",
                               "Brasil" = "black")) +
  scale_x_discrete(labels = c("2023T1.Homem Branco" = "2023T1", 
                              "2024T1.Homem Branco" = " 2024T1",
                              "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1",
                              "2023T1.Brasil" = "2023T1",
                              "2024T1.Brasil" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "(R$)",
       title = "Rendimento Efetivo Médio por Gênero e Raça")

print(r_efe_all)
dev.off()

# gph 2 - renda habitual media
pdf(file.path(figures_output, "rendimento_habitual.pdf"),  width = 12, height = 8.5)
r_hab_all <- dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = renda_media_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(renda_media_hab,2)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "Gênero e Raça",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4",
                               "Brasil" = "black")) +
  scale_x_discrete(labels = c("2023T1.Homem Branco" = "2023T1", 
                              "2024T1.Homem Branco" = " 2024T1",
                              "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1",
                              "2023T1.Brasil" = "2023T1",
                              "2024T1.Brasil" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "(R$)",
       title = "Rendimento Habitual Médio por Gênero e Raça")

print(r_hab_all)
dev.off()


# gph 3 - taxa de desemprego
pdf(file.path(figures_output, "unemp.pdf"),  width = 12, height = 8.5)
unemp <- dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = 100*tx_desocup, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(100*tx_desocup,2)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "Gênero e Raça",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4",
                               "Brasil" = "black")) +
  scale_x_discrete(labels = c("2023T1.Homem Branco" = "2023T1", 
                              "2024T1.Homem Branco" = " 2024T1",
                              "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1",
                              "2023T1.Brasil" = "2023T1",
                              "2024T1.Brasil" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "(%)",
       title = "Taxa de Desocupação por Gênero e Raça")

print(unemp)
dev.off()

# gph 4 - pea
pdf(file.path(figures_output, "pea.pdf"),  width = 12, height = 8.5)
pea <- dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = 100*pea_fac, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(100*pea_fac,2)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "Gênero e Raça",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4",
                               "Brasil" = "black")) +
  scale_x_discrete(labels = c("2023T1.Homem Branco" = "2023T1", 
                              "2024T1.Homem Branco" = " 2024T1",
                              "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1",
                              "2023T1.Brasil" = "2023T1",
                              "2024T1.Brasil" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "(%)",
       title = "População Economicamente Ativa por Gênero e Raça")

print(pea)
dev.off()

# gph 5 - massa salarial habitual
pdf(file.path(figures_output, "massa_habitual.pdf"),  width = 12, height = 8.5)
massa_hab <- dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = massa_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(massa_hab,2)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "Gênero e Raça",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4",
                               "Brasil" = "black")) +
  scale_x_discrete(labels = c("2023T1.Homem Branco" = "2023T1", 
                              "2024T1.Homem Branco" = " 2024T1",
                              "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1",
                              "2023T1.Brasil" = "2023T1",
                              "2024T1.Brasil" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "(R$ em bilhões)",
       title = "Massa Salarial Habitual por Gênero e Raça")

print(massa_hab)
dev.off()

# gph 6 - massa salarial efetiva
pdf(file.path(figures_output, "massa_efetiva.pdf"),  width = 12, height = 8.5)
massa_efe <- dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = massa_efe, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(massa_efe,2)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "Gênero e Raça",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4",
                               "Brasil" = "black")) +
  scale_x_discrete(labels = c("2023T1.Homem Branco" = "2023T1", 
                              "2024T1.Homem Branco" = " 2024T1",
                              "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1",
                              "2023T1.Brasil" = "2023T1",
                              "2024T1.Brasil" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "(R$ em bilhões)",
       title = "Massa Salarial Efetiva por Gênero e Raça")

print(massa_efe)
dev.off()

# gph 7 - gini
pdf(file.path(figures_output, "gini.pdf"),  width = 12, height = 8.5)
gini_hab <- dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = gini_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(gini_hab,2)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "Gênero e Raça",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4",
                               "Brasil" = "black")) +
  scale_x_discrete(labels = c("2023T1.Homem Branco" = "2023T1", 
                              "2024T1.Homem Branco" = " 2024T1",
                              "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1",
                              "2023T1.Brasil" = "2023T1",
                              "2024T1.Brasil" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "Índice de Gini",
       title = "Indicador de desigualdade do rendimento habitual
       individual de todos os trabalhos por gênero raça")

print(gini_hab)
dev.off()

