
dt1_long <- as.data.table(dt1_long)

dt1_long[, topo_base := as.numeric(category %in% c("b10", "b5", "b1"))]

dt1_long[, order := fcase(category %in% c("b1", "t10"), 1L,
                          category %in% c("b5", "t5"), 3L,
                          category %in% c("b10", "t1"), 5L)]

table(dt1_long$category, dt1_long$order)

dt1_long[Ano_trimestre == "2023T1", teste := order]
dt1_long[Ano_trimestre == "2024T1", teste := order + 1]


ggplot(dt1_long, 
       aes(x = teste, y = value, fill = gender_race)) +
  geom_bar(stat = 'identity', 
           position = position_dodge(width = 0.8), width = 0.7,) +
  geom_col(position = position_stack(reverse = FALSE)) +
  geom_text(aes(label = round(value)), 
            position = position_stack(vjust = 0.5), 
            size = 4.5) +
  scale_fill_manual(name = "",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  facet_wrap(~ base_topo)

ggplot(dt1_long, 
       aes(x = Ano_trimestre_category, y = value, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7,) +
  geom_col(position = position_stack(reverse = FALSE)) +
  geom_text(aes(label = round(value)), 
            position = position_stack(vjust = 0.5), 
            size = 4.5) +
  scale_fill_manual(name = "",
                    values = c("Homem Branco" = "aquamarine4",
                               "Mulher Branca" = "darkorange1",
                               "Homem Negro" = "darkgoldenrod1",
                               "Mulher Negra" = "brown4")) +
  scale_x_discrete(labels = c("2023T1.b1" = "23T1-1%", 
                              "2023T1.b5" = "23T1-5%",
                              "2023T1.b10" = "23T-10%",
                              "2023T1.t10" = "23T1-10%", 
                              "2023T1.t5" = "23T15%",
                              "2023T1.t1" = "23T1-1%",
                              "2024T1.b1" = "24T1-1%", 
                              "2024T1.b5" = "24T1-5%",
                              "2024T1.b10" = "24T1-10%",
                              "2024T1.t10" = "24T1-10%", 
                              "2024T1.t5" = "24T1-5%",
                              "2024T1.t1" = "24T1-1%")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, lineheight = 0.9),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5))  +
  labs(x = "", y = "%", title = "") 
