massa_homens <- fread(file.path(csv_output, "resultados_massa_salarial_homens.csv"))
massa_mulheres <- fread(file.path(csv_output, "resultados_massa_salarial_mulheres.csv"))

massa <- cbind(massa_homens, massa_mulheres)
mult <- c("total_hn", "total_mn",
          "composicao_massa_wg_hn", "discriminacao_massa_wg_hn",
          "composicao_massa_wg_mn", "discriminacao_massa_wg_mn",
          "composicao_massa_emp_hn", "discriminacao_massa_emp_hn",
          "composicao_massa_emp_mn", "discriminacao_massa_emp_mn")
massa <- massa[, (mult) := lapply(.SD, function(x) x * -1), .SDcols = mult]

massa[, penalidade_wg_hn := composicao_massa_wg_hn + discriminacao_massa_wg_hn]
massa[, penalidade_wg_mn := composicao_massa_wg_mn + discriminacao_massa_wg_mn]
massa[, penalidade_emp_hn := composicao_massa_emp_hn + discriminacao_massa_emp_hn]
massa[, penalidade_emp_mn := composicao_massa_emp_mn + discriminacao_massa_emp_mn]

massa <- setnames(massa, c("total_hn", "total_mn",
                           "composicao_massa_wg_hn", "discriminacao_massa_wg_hn",
                           "composicao_massa_wg_mn", "discriminacao_massa_wg_mn",
                           "penalidade_wg_hn", "penalidade_wg_mn",
                           "composicao_massa_emp_hn", "discriminacao_massa_emp_hn",
                           "composicao_massa_emp_mn", "discriminacao_massa_emp_mn",
                           "penalidade_emp_hn", "penalidade_emp_mn"),
                  c("Massa Total Perdida Homens (A+B)", "Massa Total Perdida Mulheres (A+B)", 
                    "Efeito Composição Salários Homens", "Efeito Discriminação Salários Homens",
                    "Efeito Composição Salários Mulheres", "Efeito Discriminação Salários Mulheres",
                    "Penalidade Salarial Homens (A)", "Penalidade Salarial Mulheres (A)",
                    "Efeito Composição Empregabilidade Homens",  "Efeito Discriminação Empregabilidade Homens",
                    "Efeito Composição Empregabilidade Mulheres", "Efeito Discriminação Empregabilidade Mulheres",
                    "Penalidade na Empregabilidade Homens (B)", "Penalidade na Empregabilidade Mulheres (B)"))

homens <- massa[, .(`Ano_trimestre`, `Massa Total Perdida Homens (A+B)`,
                    `Efeito Composição Salários Homens`,
                    `Efeito Discriminação Salários Homens`,
                    `Penalidade Salarial Homens (A)`,
                    `Efeito Composição Empregabilidade Homens`,
                    `Efeito Discriminação Empregabilidade Homens`,
                    `Penalidade na Empregabilidade Homens (B)`)]

homens <- homens[Ano_trimestre %in% c("2023T1", "2024T1"),]

homens_t <- setnames(homens[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                     homens[, c('variaveis', Ano_trimestre)])[]

setnames(homens_t, c("2023T1", "2024T1"), c("Homem_2023T1", "Homem_2024T1"))
homens_t$variaveis <- gsub("Homens", "", homens_t$variaveis)

mulheres <- massa[, .(`Ano_trimestre`, `Massa Total Perdida Mulheres (A+B)`,
                     `Efeito Composição Salários Mulheres`,
                     `Efeito Discriminação Salários Mulheres`,
                     `Penalidade Salarial Mulheres (A)`,
                     `Efeito Composição Empregabilidade Mulheres`,
                     `Efeito Discriminação Empregabilidade Mulheres`,
                     `Penalidade na Empregabilidade Mulheres (B)`)]

mulheres <- mulheres[Ano_trimestre %in% c("2023T1", "2024T1"),]
mulheres_t <- setnames(mulheres[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                       mulheres[, c('m', Ano_trimestre)])[]

setnames(mulheres_t, c("2023T1", "2024T1"), c("Mulher_2023T1", "Mulher_2024T1"))
#mulheres_t$variaveis <- gsub("Mulheres", "", mulheres_t$variaveis)

penalidade <- cbind(homens_t, mulheres_t)
penalidade[,m := NULL]
penalidade <- penalidade[, Total_2023T1 := Homem_2023T1 + Mulher_2023T1]
penalidade <- penalidade[, Total_2024T1 := Homem_2024T1 + Mulher_2024T1]

penalidade <- setcolorder(penalidade, 
                      c("variaveis", "Homem_2023T1", "Mulher_2023T1",
                        "Total_2023T1", "Homem_2024T1", "Mulher_2024T1",
                        "Total_2024T1"))
penalidade <- penalidade[c(1, 4, 2, 3, 7, 5, 6), ]

table_penalidade <-
  kable(head(penalidade, 7), align = 'l', booktabs = TRUE, 
        col.names = rep("", ncol(penalidade)), format = 'latex') %>% 
  column_spec(1, width = "250px") %>%  
  column_spec(c(2:7), width = "60px") %>% 
  row_spec(c(1,2,5), bold = TRUE) %>% 
  row_spec(c(1:7), color = "black") %>% 
  add_header_above(c("", "Homem" = 1, "Mulher" = 1, "Total" = 1,
                     "Homem" = 1, "Mulher" = 1, "Total" = 1)) %>% 
  add_header_above(c("", "2023" = 3, "2024" = 3)) %>% 
  add_indent(c(3, 4, 6, 7), level_of_indent = 2) 

writeLines(table_penalidade, file.path(outputs, "penalidade_negros_br.tex"))



