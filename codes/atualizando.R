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

