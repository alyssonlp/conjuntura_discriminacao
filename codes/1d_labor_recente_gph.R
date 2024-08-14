dt <- fread(file.path(csv_output, "descriptive_br_gender_race_carta.csv"))

dt1 <- dt %>%  
  filter(Ano_trimestre %in% c("2023T2", "2024T2"))

breaks_seq <- c("Brasil",  "Invisivel", "Homem Branco", "Mulher Branca",
                "Homem Negro", "Mulher Negra") 

labels_seq <- c("Brasil",  "", "Homem Branco", "Mulher Branca",
                "Homem Negro", "Mulher Negra") 

color_squeme <- c("Homem Branco" = "aquamarine4",
                  "Mulher Branca" = "darkorange1",
                  "Invisivel" = "transparent",
                  "Homem Negro" = "darkgoldenrod1",
                  "Mulher Negra" = "brown4",
                  "Brasil" = "black")

linetype_squeme <- c("Homem Branco" ="solid",
                     "Mulher Branca" =  "solid", 
                     "Invisivel" = "solid",
                     "Homem Negro" = "solid", 
                     "Mulher Negra" ="solid",
                     "Brasil" = "solid")

# gph 1 - renda habitual media
pdf(file.path(figures_output, "rendimento_habitual.pdf"),  width =14, height = 8.5)
r_hab_all <- dt1 %>% 
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), 
             y = renda_media_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = ifelse(renda_media_hab == 0, "", round(renda_media_hab))), 
            vjust = -0.5,
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "", 
                    values = color_squeme,
                    breaks = breaks_seq,
                    labels = labels_seq,
                    drop = FALSE) + 
  scale_x_discrete(limits = c("2023T2.Brasil", "2024T2.Brasil",
                              "2023T2.Homem Branco", "2024T2.Homem Branco",
                              "2023T2.Mulher Branca", "2024T2.Mulher Branca",
                              "2023T2.Homem Negro", "2024T2.Homem Negro",
                              "2023T2.Mulher Negra", "2024T2.Mulher Negra"),
                   labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = "2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024",
                              "2023T2.Brasil" = "2023",
                              "2024T2.Brasil" = "2024",
                              "2023T2.Invisivel" = "",
                              "2024T2.Invisivel" = "")) +
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 1000)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        legend.title = element_text(size = 34),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), 
        legend.text = element_text(size = 28),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  labs(x = "", y = "R$", title = "")

print(r_hab_all)
dev.off()


# gph 2 - taxa de desemprego
pdf(file.path(figures_output, "unemp.pdf"),  width = 14, height = 8.5)
unemp <- dt1 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race),
             y = 100*tx_desocup, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(100*tx_desocup,2)), vjust = -0.5, 
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "", 
                    values = color_squeme,
                    breaks = breaks_seq,
                    labels = labels_seq,
                    drop = FALSE) + 
  scale_x_discrete(limits = c("2023T2.Brasil", "2024T2.Brasil",
                              "2023T2.Homem Branco", "2024T2.Homem Branco",
                              "2023T2.Mulher Branca", "2024T2.Mulher Branca",
                              "2023T2.Homem Negro", "2024T2.Homem Negro",
                              "2023T2.Mulher Negra", "2024T2.Mulher Negra"),
                   labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = "2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024",
                              "2023T2.Brasil" = "2023",
                              "2024T2.Brasil" = "2024",
                              "2023T2.Invisivel" = "",
                              "2024T2.Invisivel" = "")) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.title = element_text(size = 34),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 28),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  labs(x = "", y = "%", title = "")

print(unemp)
dev.off()

# gph 3 - pea
pdf(file.path(figures_output, "pea.pdf"),  width =14, height = 8.5)
pea <- dt1 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), 
             y = 100*pea_fac, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(100*pea_fac)), vjust = -0.5, 
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "", 
                    values = color_squeme,
                    breaks = breaks_seq,
                    labels = labels_seq,
                    drop = FALSE) + 
  scale_x_discrete(limits = c("2023T2.Brasil", "2024T2.Brasil",
                              "2023T2.Homem Branco", "2024T2.Homem Branco",
                              "2023T2.Mulher Branca", "2024T2.Mulher Branca",
                              "2023T2.Homem Negro", "2024T2.Homem Negro",
                              "2023T2.Mulher Negra", "2024T2.Mulher Negra"),
                   labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = "2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024",
                              "2023T2.Brasil" = "2023",
                              "2024T2.Brasil" = "2024",
                              "2023T2.Invisivel" = "",
                              "2024T2.Invisivel" = "")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        legend.title = element_text(size = 34),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 28),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  labs(x = "", y = "%", title = "")

print(pea)
dev.off()

# gph 4 - massa salarial habitual
dt2 <- dt1 %>%  
  filter(gender_race != "Brasil" & gender_race != "Invisivel") 
dt2[, massa_hab := massa_hab/1000000000]


pdf(file.path(figures_output, "massa_habitual.pdf"),  width =14, height = 8.5)
massa_hab <- dt2 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), 
             y = massa_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(massa_hab)), vjust = -0.5, 
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = " 2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024")) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        legend.title = element_text(size = 34),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 28),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
  labs(x = "", y = "R$ em bilhões", title = "")

print(massa_hab)
dev.off()

# gph 5 - gini
pdf(file.path(figures_output, "gini.pdf"),  width =14, height = 8.5)
gini_hab <- dt1 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), 
             y = gini_hab, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(gini_hab,2)), vjust = -0.5, 
            position = position_dodge(width = 0.8), size = 8) +
  scale_fill_manual(name = "", 
                    values = color_squeme,
                    breaks = breaks_seq,
                    labels = labels_seq,
                    drop = FALSE) + 
  scale_x_discrete(limits = c("2023T2.Brasil", "2024T2.Brasil",
                              "2023T2.Homem Branco", "2024T2.Homem Branco",
                              "2023T2.Mulher Branca", "2024T2.Mulher Branca",
                              "2023T2.Homem Negro", "2024T2.Homem Negro",
                              "2023T2.Mulher Negra", "2024T2.Mulher Negra"),
                   labels = c("2023T2.Homem Branco" = "2023", 
                              "2024T2.Homem Branco" = "2024",
                              "2023T2.Mulher Branca" = "2023",
                              "2024T2.Mulher Branca" = "2024",
                              "2023T2.Homem Negro" = "2023",
                              "2024T2.Homem Negro" = "2024",
                              "2023T2.Mulher Negra" = "2023",
                              "2024T2.Mulher Negra" = "2024",
                              "2023T2.Brasil" = "2023",
                              "2024T2.Brasil" = "2024",
                              "2023T2.Invisivel" = "",
                              "2024T2.Invisivel" = "")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.20)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        legend.title = element_text(size = 34),
        axis.text.x = element_text( vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=28),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE)) + 
  labs(x = "", y = "Índice de Gini", title = "")

print(gini_hab)
dev.off()

