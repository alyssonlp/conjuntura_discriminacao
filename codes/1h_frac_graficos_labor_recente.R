dt <- fread(file.path(csv_files, "descriptive_br_gender_race_carta.csv"))

homem_branco_results <- dt[gender_race=="Homem Branco"]

homem_branco_results <- setnames(homem_branco_results,
                                 c("gender_race", "renda_media_hab",
                                   "renda_media_efe", "massa_hab",
                                   "massa_efe", "tx_desocup", "pea_fac",
                                   "gini_hab", "massa_hab_inter",
                                   "massa_efe_inter"),
                                 c("homem_branco", "hb_renda_media_hab",
                                   "hb_renda_media_efe", "hb_massa_hab",
                                   "hb_massa_efe", "hb_tx_desocup", "hb_pea_fac",
                                   "hb_gini_hab", "hb_massa_hab_inter",
                                   "hb_massa_efe_inter"))

join_dt <- merge(dt, homem_branco_results, by = "Ano_trimestre", all.x = TRUE)

join_dt <- join_dt[, frac_renda_media_hab := (renda_media_hab/hb_renda_media_hab)*100]
join_dt <- join_dt[, frac_renda_media_efe := (renda_media_efe/hb_renda_media_efe)*100]
join_dt <- join_dt[, frac_massa_hab := (massa_hab/hb_massa_hab)*100]
join_dt <- join_dt[, frac_massa_efe := (massa_efe/hb_massa_efe)*100]
join_dt <- join_dt[, frac_massa_hab_inter := (massa_hab_inter/hb_massa_hab_inter)*100]
join_dt <- join_dt[, frac_massa_efe_inter := (massa_efe_inter/hb_massa_efe_inter)*100]
join_dt <- join_dt[, frac_gini_hab := gini_hab/hb_gini_hab]
join_dt <- join_dt[, frac_unemp := tx_desocup/hb_tx_desocup]
join_dt <- join_dt[, frac_pea := (pea_fac/hb_pea_fac)*100]


# gph 1 - renda efetiva media - razao

pdf(file.path(figures_output, "frac_rendimento_efetivo.pdf"),  width = 12, height = 8.5)
frac_r_efe_all <- join_dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  filter(gender_race %in% c("Mulher Branca", "Homem Negro", "Mulher Negra" )) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = frac_renda_media_efe, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(frac_renda_media_efe)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "",
                    values = c("Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "%",title = "")

print(frac_r_efe_all)
dev.off()

# gph 2 - renda habitual media
pdf(file.path(figures_output, "frac_rendimento_habitual.pdf"),  width = 12, height = 8.5)
frac_r_hab_all <- join_dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  filter(gender_race %in% c("Mulher Branca", "Homem Negro", "Mulher Negra" )) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = frac_renda_media_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(frac_renda_media_hab)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "",
                    values = c("Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "%", title = "")

print(frac_r_hab_all)
dev.off()

# gph 3 - taxa de desemprego
pdf(file.path(figures_output, "frac_unemp.pdf"),  width = 12, height = 8.5)
frac_unemp <- join_dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  filter(gender_race %in% c("Mulher Branca", "Homem Negro", "Mulher Negra" )) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = frac_unemp, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(frac_unemp)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "",
                    values = c("Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "Razão de chances",title = "")

print(frac_unemp)
dev.off()


# gph 4 - pea - frac
pdf(file.path(figures_output, "frac_pea.pdf"),  width = 12, height = 8.5)
frac_pea <- join_dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  filter(gender_race %in% c("Mulher Branca", "Homem Negro", "Mulher Negra" )) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = frac_pea, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(frac_pea)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "",
                    values = c( "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c( "2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "%", title = "")

print(frac_pea)
dev.off()

# gph 5 - massa salarial habitual - frac
pdf(file.path(figures_output, "frac_massa_habitual.pdf"),  width = 12, height = 8.5)
frac_massa_hab <- join_dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  filter(gender_race %in% c("Mulher Branca", "Homem Negro", "Mulher Negra" )) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = frac_massa_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(frac_massa_hab)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "",
                    values = c("Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "%",title = "")

print(frac_massa_hab)
dev.off()

# gph 6 - massa salarial efetiva - frac
pdf(file.path(figures_output, "frac_massa_efetiva.pdf"),  width = 12, height = 8.5)
frac_massa_efe <- join_dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  filter(gender_race %in% c("Mulher Branca", "Homem Negro", "Mulher Negra" )) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = frac_massa_efe, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(frac_massa_efe)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "",
                    values = c("Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "%",title = "")

print(frac_massa_efe)
dev.off()


# gph 7 - gini
pdf(file.path(figures_output, "frac_gini.pdf"),  width = 12, height = 8.5)
frac_gini_hab <- join_dt %>%
  filter(Ano_trimestre %in% c("2023T1", "2024T1")) %>%
  filter(gender_race %in% c("Mulher Branca", "Homem Negro", "Mulher Negra" )) %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = frac_gini_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(frac_gini_hab)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(name = "",
                    values = c("Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T1.Mulher Branca" = "2023T1",
                              "2024T1.Mulher Branca" = "2024T1",
                              "2023T1.Homem Negro" = "2023T1",
                              "2024T1.Homem Negro" = "2024T1",
                              "2023T1.Mulher Negra" = "2023T1",
                              "2024T1.Mulher Negra" = "2024T1")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22)) +
  labs(x = "", y = "Razão de Chances", title = "")

print(frac_gini_hab)
dev.off()
