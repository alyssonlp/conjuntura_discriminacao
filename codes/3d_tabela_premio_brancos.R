massa_homens <- fread(file.path(csv_output, "resultados_massa_salarial_homens.csv"))
massa_mulheres <- fread(file.path(csv_output, "resultados_massa_salarial_mulheres.csv"))

massa <- cbind(massa_homens, massa_mulheres)

# Criando totais: homem branco + mulher branca
massa[, total_b := total_hb + total_mb]
massa[, composicao_wg_b := composicao_massa_wg_hb + composicao_massa_wg_mb]
massa[, composicao_emp_b := composicao_massa_emp_hb + composicao_massa_emp_mb ]

massa <- setnames(massa, c("total_b", "total_hb", "total_mb",
                  "composicao_wg_b", "composicao_massa_wg_hb", "composicao_massa_wg_mb",
                  "composicao_emp_b", "composicao_massa_emp_hb", "composicao_massa_emp_mb"),
                  c("Massa Total Premiada", "Massa Total Premiada Homens",
                    "Massa Total Premiada Mulheres", "Efeito Composição Salários Homens",
                    "Efeito Composição Salários Mulheres", "Efeito COmposição Salários",
                    "Efeito Composição Empregabilidade Homens",
                    "Efeito Composição Empregabilidade Mulheres", "Efeito Empregabilidade Salários"))

homens <- massa[, .(`Ano_trimestre`, `Massa Total Premiada Homens`,
                      `Efeito Composição Salários Homens`,
                     `Efeito Composição Empregabilidade Homens`)]

homens <- homens[Ano_trimestre %in% c("2023T2", "2024T2"),]

homens_t <- setnames(homens[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                     homens[, c('variaveis', Ano_trimestre)])[]

setnames(homens_t, c("2023T2", "2024T2"), c("Homem_2023T2", "Homem_2024T2"))
homens_t$variaveis <- gsub("Homens", "", homens_t$variaveis)


mulheres <- massa[, .(`Ano_trimestre`, `Massa Total Premiada Mulheres`,
                      `Efeito Composição Salários Mulheres`,
                      `Efeito Composição Empregabilidade Mulheres`)]

mulheres <- mulheres[Ano_trimestre %in% c("2023T2", "2024T2"),]
mulheres_t <- setnames(mulheres[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                       mulheres[, c('m', Ano_trimestre)])[]

setnames(mulheres_t, c("2023T2", "2024T2"), c("Mulher_2023T2", "Mulher_2024T2"))
#mulheres_t$variaveis <- gsub("Mulheres", "", mulheres_t$variaveis)

premio <- cbind(homens_t, mulheres_t)
premio[,m := NULL]
premio <- premio[, Total_2023T2 := Homem_2023T2 + Mulher_2023T2]
premio <- premio[, Total_2024T2 := Homem_2024T2 + Mulher_2024T2]

premio <- setcolorder(premio, 
                             c("variaveis", "Homem_2023T2", "Mulher_2023T2",
                              "Total_2023T2", "Homem_2024T2", "Mulher_2024T2",
                              "Total_2024T2"))

premio_brancos <-
  kable(head(premio, 7), align = 'l', booktabs = TRUE, 
        col.names = rep("", ncol(premio)), format = 'latex') %>% 
  column_spec(1, width = "250px") %>%  
  column_spec(c(2:3), width = "60px") %>% 
  row_spec(c(1), color = "black", bold=  TRUE) %>% 
  row_spec(c(2:3), color = "black") %>% 
  add_header_above(c("", "Homem" = 1, "Mulher" = 1, "Total" = 1, "Homem" = 1, "Mulher" = 1, "Total" = 1)) %>% 
  add_header_above(c("", "2023" = 3, "2024" = 3)) %>% 
  add_indent(c(2, 3), level_of_indent = 2) 
writeLines(premio_brancos, file.path(outputs, "premio_brancos.tex"))
