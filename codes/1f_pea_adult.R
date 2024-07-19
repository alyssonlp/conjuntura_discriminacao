dt0 <- data.table()
dt1 <- data.table()
ano <- c(2012:2024)
trimestre <- c(1:4)

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=2){
      next  
    }
    
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
    dt <- dt[V2009 %in%(25:54),]

 anotri <- sprintf("%dT%d", aa, tri)
 pea_fac <- dt[,wtd.mean(pea, weights = V1028), by= c("gender_race")] 
 pea_fac_br <- dt[,wtd.mean(pea, weights = V1028)] 
 dt2 <- data.table(Ano_trimestre = anotri, pea_fac_adult = pea_fac)
 dt3 <- data.table(Ano_trimestre = anotri, pea_fac_adult = pea_fac_br)
 dt0 <- rbind(dt0, dt2, fill = TRUE)
 dt1 <- rbind(dt1, dt3, fill = TRUE)
  }
}
dt0 <- setnames(dt0, c("pea_fac_adult.gender_race", "pea_fac_adult.V1"),
                c("gender_race", "pea_fac"))
dt1 <- setnames(dt1, c( "pea_fac_adult"),
                c( "pea_fac"))
dt1 <- dt1[, gender_race := "Brasil"]
dt1 <- rbind(dt0, dt1, fill = TRUE)
fwrite(dt1, file.path(csv_output, "pea_adult.csv"))

dt1 <- dt1 %>%  
  filter(Ano_trimestre %in% c("2023T1", "2024T1"))

pdf(file.path(figures_output, "pea_adult.pdf"),  width =14, height = 8.5)
pea_adult <- dt1 %>%
  ggplot(aes(x = interaction(Ano_trimestre, gender_race), y = 100*pea_fac, fill = gender_race)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(100*pea_fac)), vjust = -0.5, position = position_dodge(width = 0.8), size = 5) +
  scale_fill_manual(name = "",
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
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 22),
        legend.position = "bottom",
        legend.title = element_text(size = 30),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size=22),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  labs(x = "", y = "%", title = "")

print(pea_adult)
dev.off()