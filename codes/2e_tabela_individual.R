dt <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

dt2 <- dt[Ano_trimestre %in% c("2023T1", "2024T1"),]

dt2[, penalidade_salarial_hn := composicao_wg_hn + discriminacao_wg_hn]
dt2[, penalidade_emp_hn := composicao_emp_hn + discriminacao_emp_hn ]
dt2[, penalidade_salarial_mn := composicao_wg_mn + discriminacao_wg_mn]
dt2[, penalidade_emp_mn := composicao_emp_mn + discriminacao_emp_mn ]

mult <- c("composicao_wg_hn", "discriminacao_wg_hn",
          "composicao_emp_hn", "discriminacao_emp_hn", 
          "penalidade_salarial_hn", "penalidade_emp_hn",
          "composicao_wg_mn", "discriminacao_wg_mn",
          "composicao_emp_mn", "discriminacao_emp_mn" , 
          "penalidade_salarial_mn", "penalidade_emp_mn")

dt2 <- dt2[, (mult) := lapply(.SD, function(x) x * -1), .SDcols = mult]

# Tabela 1 - resultados perda da massa salarial
dt_homem_e <- dt2[, .(Ano_trimestre, composicao_wg_hn, discriminacao_wg_hn,
                      composicao_emp_hn, discriminacao_emp_hn,
                      penalidade_salarial_hn, penalidade_emp_hn)]

dt_mulher_e <- dt2[, .(Ano_trimestre, composicao_wg_mn, discriminacao_wg_mn,
                       composicao_emp_mn, discriminacao_emp_mn, 
                       penalidade_salarial_mn, penalidade_emp_mn)]

setnames(dt_homem_e, 
         c("composicao_wg_hn", "discriminacao_wg_hn","composicao_emp_hn", 
           "discriminacao_emp_hn", "penalidade_salarial_hn","penalidade_emp_hn" ), 
         c( "Efeito Composição Salário HN","Efeito Discriminação Salário HN",
            "Efeito Composição da Empregabilidade HN",
            "Efeito Discriminação da Empregabilidade HN",
            "Penalidade Salarial HN", "Penalidade na Empregabilidade HN" ))

setnames(dt_mulher_e, 
         c("composicao_wg_mn", "discriminacao_wg_mn","composicao_emp_mn", 
           "discriminacao_emp_mn", "penalidade_salarial_mn", "penalidade_emp_mn"), 
         c("Efeito Composição Salário MN","Efeito Discriminação Salário MN", 
           "Efeito Composição da Empregabilidade MN",
           "Efeito Discriminação da Empregabilidade MN",
           "Penalidade Salarial MN", "Penalidade na Empregabilidade MN" ))


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
individual_recente <- individual_recente[c(5, 1, 2, 6, 3, 4), ]

# Criando a tabela 
table_individual <-
  kable(head(individual_recente, 6), align = 'l', booktabs = TRUE, 
        col.names = rep("", ncol(individual_recente)), format = 'latex') %>% 
  column_spec(1, width = "250px") %>%  
  column_spec(c(2:5), width = "60px") %>% 
  row_spec(c(1,4), bold = TRUE) %>% 
  row_spec(c(1:6), color = "black") %>% 
  add_header_above(c("", "Homem" = 1, "Mulher" = 1,  "Homem" = 1, "Mulher" = 1)) %>% 
  add_header_above(c("", "2023" = 2, "2024" = 2)) %>% 
  add_indent(c(2,3,5,6), level_of_indent = 2) 

writeLines(table_individual, file.path(outputs, "table_individual.tex"))
