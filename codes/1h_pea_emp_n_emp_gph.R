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
    
    dt <- setDT(dt)[V2009 >= 25 & V2009 <= 65]
    
    result <- 
      dt[ , .(prop_pea_emp = wtd.mean(pea_emp, weights = V1028, na.rm = TRUE),
              prop_pea = wtd.mean(pea, weights = V1028, na.rm = TRUE),
              prop_emp = wtd.mean(emp, weights = V1028, na.rm = TRUE)),
          by = c("male", "nonwhite")]
    
    result[, diff_pea_emp := prop_pea_emp*prop_emp]
    result[, diff_pea_emp_plot := prop_pea_emp - diff_pea_emp]
    result_long <- melt.data.table(result, id.vars = c("male", "nonwhite"))
    result_long[,gender_male := interaction(male, nonwhite)]

    result_long[, gender_race := 
         case_when(
           male == 1 & nonwhite == 0 ~ "Homem Branco",
           male == 1 & nonwhite == 1 ~ "Homem Negro",
           male == 0 & nonwhite == 0 ~ "Mulher Branca",
           male == 0 & nonwhite == 1 ~ "Mulher Negra")]
    
    pdf(file.path(figures_output, "pea_emp_n_emp.pdf"),  width = 14, height = 8.5)    
    frac_pea_emp <- result_long[variable %in% c("diff_pea_emp_plot", "diff_pea_emp"), ] %>% 
      ggplot(aes(x = gender_race, y = value*100, fill = variable)) +
      geom_bar(stat = 'identity', position = "stack",
               width = 0.6, alpha = 0.8) +  # Ajustando a opacidade aqui
      geom_text(aes(label = round(value*100)), 
                position = position_stack(vjust = 0.5), 
                size = 8, fontface = "bold") +
      scale_fill_manual(name = "",
                        values = c("diff_pea_emp_plot" = "coral3",
                                   "diff_pea_emp" = "chocolate1"),
                        labels = c("Empregados", "Desempregados")) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
      theme_classic() +
      theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
            text = element_text(size = 34),
            legend.position = "bottom",
            legend.title = element_text(size = 34),
            axis.text.x = element_text(vjust = 0.5, hjust = 0.5, color = "black"),
            plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 28),
            plot.margin = margin(t = 5, r = 22, b = 5, l = 5)) +
      guides(fill = guide_legend(nrow = 1)) + 
      labs(x = "", y = "%", title = "")

    print(frac_pea_emp)
    dev.off()
  }
}