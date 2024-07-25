dt <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

dt1 <- dt[Ano_trimestre %in% c("2023T1", "2024T1"),]
mult <- c("composicao_hn_wg", "discriminacao_hn_wg",
          "composicao_hn_emp", "discriminacao_hn_emp", "penalidade_salarial_hn",
          "composicao_mn_wg", "discriminacao_mn_wg",
          "composicao_mn_emp", "discriminacao_mn_emp" , "penalidade_salarial_mn")
dt1 <- dt1[, (mult) := lapply(.SD, function(x) x * -1), .SDcols = mult]

# Tabela 1 - resultados perda da massa salarial
dt_homem_e <- dt1[, .(Ano_trimestre, composicao_hn_wg, discriminacao_hn_wg,
                     composicao_hn_emp, discriminacao_hn_emp, penalidade_salarial_hn)]

dt_mulher_e <- dt1[, .(Ano_trimestre, composicao_mn_wg, discriminacao_mn_wg,
                       composicao_mn_emp, discriminacao_mn_emp, penalidade_salarial_mn)]

 setnames(dt_homem_e, 
           c("composicao_hn_wg", "discriminacao_hn_wg","composicao_hn_emp", 
             "discriminacao_hn_emp", "penalidade_salarial_hn"), 
           c( "Efeito Composição Salário HN","Efeito Discriminação Salário HN",
              "Efeito Composição da Empregabilidade HN",
          "Efeito Discriminação da Empregabilidade HN", "Penalidade Salarial HN" ))

 setnames(dt_mulher_e, 
          c("composicao_mn_wg", "discriminacao_mn_wg","composicao_mn_emp", 
          "discriminacao_mn_emp", "penalidade_salarial_mn"), 
          c("Efeito Composição Salário MN","Efeito Discriminação Salário MN", 
            "Efeito Composição da Empregabilidade MN",
           "Efeito Discriminação da Empregabilidade MN", "Penalidade Salarial MN"))
 

# Transpor cada data.table
# Para homens

homens_e <- setnames(dt_homem_e[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                     dt_homem_e[, c('variaveis', Ano_trimestre)])[]
setnames(homens_e, c("2023T1", "2024T1"), c("Homem_2023T1", "Homem_2024T1"))
homens_e$variaveis <- gsub("HN", " ", homens_e$variaveis)


mulheres_e <- setnames(dt_mulher_e[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                       dt_mulher_e[, c('variaveis_m', Ano_trimestre)])[]
setnames(mulheres_e, c("2023T1", "2024T1"), c("Mulher_2023T1", "Mulher_2024T1"))


individual_recente <- cbind(homens_e, mulheres_e)
individual_recente <- individual_recente[, variaveis_m := NULL]

individual_recente <- setcolorder(individual_recente,c("variaveis", "Homem_2023T1", "Mulher_2023T1",
                                                        "Homem_2024T1", "Mulher_2024T1"))
individual_recente <- setnames(individual_recente, c("variaveis", "Homem_2023T1", "Mulher_2023T1",
                                                      "Homem_2024T1", "Mulher_2024T1"),
                               c("Resultados", "Homem 2023T1", "Mulher 2023T1",
                                  "Homem 2024T1", "Mulher 2024T1"))

individual_recente <- individual_recente[c(5, 1:4, 6:nrow(individual_recente)), ]
individual_recente <- individual_recente[1:5,]

# Criando a tabela 
table_individual <-
  kable(head(individual_recente, 5), align = 'l', booktabs = TRUE, col.names = rep("", ncol(individual_recente))) %>% 
  column_spec(1, width = "300px") %>%  
  column_spec(c(2:5), width = "100px") %>% 
  row_spec(1, bold = FALSE, background = "lightgray") %>% 
  add_header_above(c("", "Homem" = 1, "Mulher" = 1,  "Homem" = 1, "Mulher" = 1)) %>% 
  add_header_above(c("", "2023" = 2, "2024" = 2)) %>% 
  pack_rows("Diferença Salarial", 2, 3) %>% 
  pack_rows("Diferença na empregabilidade", 4, 5) 


save_kable(table_individual, file.path(tables_output, file = "table_individual.html"))
webshot(file.path(tables_output,"table_individual.html"), file.path(tables_output, "table_individual.pdf"))
