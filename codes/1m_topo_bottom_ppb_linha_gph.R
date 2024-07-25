dt <- fread( file.path(csv_output, "top_bottom_ppb.csv"))
dt$raca <- factor(dt$raca, levels = c("Homem Branco", "Mulher Branca", "Homem Pardo", 
                                      "Mulher Parda", "Homem Preto", "Mulher Preta"))


# Para o 1%
# Base 1%
base1 <- dt[, .(raca, Ano_trimestre, b1)]
base1 <- base1[, Grupo := "Base"]
base1 <- setnames(base1, "b1", "prop")
# Topo 1%
topo1 <- dt[, .(raca, Ano_trimestre, t1)]
topo1 <- topo1[, Grupo := "Topo"]
topo1 <- setnames(topo1, "t1", "prop")
# Juntando
one <- rbind(base1, topo1)

pdf(file.path(figures_output, "base_topo_1_ppb_linha.pdf"),  width = 14, height = 8.5)
um_porcento <- one %>% 
  ggplot(aes(x = Ano_trimestre, y = prop, group = raca, color = raca)) + 
  geom_line(size = 1.6) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "aquamarine4",
                                "Mulher Branca" = "aquamarine3",
                                "Homem Pardo" = "chocolate3",
                                "Mulher Parda" = "chocolate1",
                                "Homem Preto" = "orange3",
                                "Mulher Preta" = "goldenrod1")) +
  scale_x_discrete(breaks = c("2012T1", "2016T1", "2020T1", "2024T1"),
                   labels = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(3, "lines"))  +
  labs(x = "", y =  "%", title = "") +
  facet_grid(~ Grupo)

print(um_porcento)
dev.off()

# Para o 5%
# Base 5%
base5 <- dt[, .(raca, Ano_trimestre, b5)]
base5 <- base5[, Grupo := "Base"]
base5 <- setnames(base5, "b5", "prop")
# Topo 5%
topo5 <- dt[, .(raca, Ano_trimestre, t5)]
topo5 <- topo5[, Grupo := "Topo"]
topo5 <- setnames(topo5, "t5", "prop")
# Juntando
five <- rbind(base5, topo5)

pdf(file.path(figures_output, "base_topo_5_ppb_linha.pdf"),  width = 14, height = 8.5)
cinco <- five %>% 
  ggplot(aes(x = Ano_trimestre, y = prop, group = raca, color = raca)) + 
  geom_line(size = 1.6) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "aquamarine4",
                                "Mulher Branca" = "aquamarine3",
                                "Homem Pardo" = "chocolate3",
                                "Mulher Parda" = "chocolate1",
                                "Homem Preto" = "orange3",
                                "Mulher Preta" = "goldenrod1")) +
  scale_x_discrete(breaks = c("2012T1", "2016T1", "2020T1", "2024T1"),
                   labels = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(3, "lines"))  +
  labs(x = "", y =  "%", title = "") +
  facet_grid(~ Grupo)

print(cinco)
dev.off()

# Para o 10%
# Base 10%
base10 <- dt[, .(raca, Ano_trimestre, b10)]
base10 <- base10[, Grupo := "Base"]
base10 <- setnames(base10, "b10", "prop")
# Topo 10%
topo10 <- dt[, .(raca, Ano_trimestre, t10)]
topo10 <- topo10[, Grupo := "Topo"]
topo10 <- setnames(topo10, "t10", "prop")
# Juntando
ten <- rbind(base10, topo10)

pdf(file.path(figures_output, "base_topo_10_ppb_linha.pdf"),  width = 14, height = 8.5)
dez <- ten %>% 
  ggplot(aes(x = Ano_trimestre, y = prop, group = raca, color = raca)) + 
  geom_line(size = 1.6) +
  scale_color_manual(name = "", 
                     values = c("Homem Branco" = "aquamarine4",
                                "Mulher Branca" = "aquamarine3",
                                "Homem Pardo" = "chocolate3",
                                "Mulher Parda" = "chocolate1",
                                "Homem Preto" = "orange3",
                                "Mulher Preta" = "goldenrod1")) +
  scale_x_discrete(breaks = c("2012T1", "2016T1", "2020T1", "2024T1"),
                   labels = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(3, "lines"))  +
  labs(x = "", y =  "%", title = "") +
  facet_grid(~ Grupo)

print(dez)
dev.off()
