rm(list = ls()[which(!ls() %in% list_objects_to_keep)])
gc()


dt1 <- data.table()

# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2012:2024)
trimestre <- c(1:4)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=2){
      next  
    }
    
    aa = 2024
    tri = 1
    
    # Incluir avisos:
    print(paste0("Computing statistics for year ", aa, 
                 " and quarter ", tri))
    
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
    result <- 
      dt[ , .(prop_pea_emp = wtd.mean(pea_emp, weights = V1028, na.rm = TRUE),
                prop_pea = wtd.mean(pea, weights = V1028, na.rm = TRUE),
                prop_emp = wtd.mean(emp, weights = V1028, na.rm = TRUE)),
        by = c("male", "nonwhite")]
  
    result[, diff_pea_emp := prop_pea_emp*prop_emp]
    result[, diff_pea_emp_plot := prop_pea_emp - diff_pea_emp]
    result_long <- melt.data.table(result, id.vars = c("male", "nonwhite"))
    result_long[gender_male := interaction(male, nonwhite)]
    
    result_long[variable %in% c("diff_pea_emp_plot", "diff_pea_emp"), ] |>
      ggplot(aes(x = interaction(male, nonwhite), y = value, fill =  variable)) +
      geom_bar(position = 'stack', stat="identity")

  }
}



gen_raca <- fread(file.path(csv_output, "resultados_genero_raca_carta.csv"))
brasil <- fread(file.path(csv_output, "resultados_brasil_carta.csv"))
brasil[, gender_race := "Brasil"]
gen_raca <- gen_raca[, gender_race := gsub("_", " ", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("homem", "Homem", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("mulher", "Mulher", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negro", "Negro", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("negra", "Negra", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branco", "Branco", gender_race)]
gen_raca <- gen_raca[, gender_race := gsub("branca", "Branca", gender_race)]


br_gen_raca <- rbind(gen_raca, brasil)

# Incluindo dado para ficar invisivel:
brasil_inv <- brasil
brasil_inv[, gender_race := "Invisivel"]  

# mudando valor da variavel para ficar não ficar sobreposto com a original.
# Fazer o mesmo para outras variáveis. Talvez o melhor valor não seja zero, 
# vai depender do contexto. Testar aos poucos.
brasil_inv[, renda_media_hab := 0]  
br_gen_raca <- rbind(br_gen_raca, brasil_inv)

breaks_seq <- c("Brasil",  "Invisivel", "Homem Branco", "Mulher Branca",
  "Homem Negro", "Mulher Negra") # altera a sequencia em que os grupos aparecem
                                # Ggplot preenche de cima pra baixo

labels_seq <- c("Brasil",  "", "Homem Branco", "Mulher Branca",
                "Homem Negro", "Mulher Negra") # Muda o rotulo na legenda

color_squeme <- c("Homem Branco" = "aquamarine4",
                  "Mulher Branca" = "darkorange1",
                  "Invisivel" = "transparent", # muda cor para transparente
                  "Homem Negro" = "darkgoldenrod1",
                  "Mulher Negra" = "brown4",
                  "Brasil" = "black")

linetype_squeme <- c("Homem Branco" ="solid",
                     "Mulher Branca" =  "solid", 
                     "Invisivel" = "solid",
                     "Homem Negro" = "solid", 
                     "Mulher Negra" ="solid",
                     "Brasil" = "solid")

br_gen_raca_r_hab_all <- 
  br_gen_raca %>% 
  ggplot(aes(x = Ano_trimestre, y = renda_media_hab, 
             color = gender_race, group = gender_race)) + 
  geom_line(size = 3.0, aes(linetype = gender_race)) +
  scale_color_manual(name = "", 
                     values = color_squeme,
                     breaks = breaks_seq,
                     labels = labels_seq)+
  scale_linetype_manual(name = "", 
                        values = linetype_squeme,
                        breaks = breaks_seq,
                        labels = labels_seq) +
  scale_x_discrete(breaks = c("2012T1", "2016T1", "2020T1","2024T1"), 
                   label = c("2012", "2016", "2020", "2024")) +
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 1000)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        text = element_text(size = 34),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"), # Isso aqui estava errado.
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 34),
        plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2), 
         color = guide_legend(nrow = 2)) + 
  labs(x = "", y = "R$",title = "")

br_gen_raca_r_hab_all
