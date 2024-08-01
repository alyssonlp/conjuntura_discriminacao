dt <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

dt1 <- dt[Ano_trimestre %in% c("2023T1", "2024T1"),]
dt1[, massa_wg_perdida_hn := composicao_massa_wg_hn + discriminacao_massa_wg_hn]
dt1[, massa_emp_perdida_hn := composicao_massa_emp_hn + discriminacao_massa_emp_hn]
dt1[, massa_wg_perdida_mn := composicao_massa_wg_mn + discriminacao_massa_wg_mn]
dt1[, massa_emp_perdida_mn := composicao_massa_emp_mn + discriminacao_massa_emp_mn]


mult <- c("total_hn","massa_wg_perdida_hn" ,
          "composicao_massa_wg_hn", "discriminacao_massa_wg_hn", 
          "massa_emp_perdida_hn",
          "composicao_massa_emp_hn", "discriminacao_massa_emp_hn",
          "total_mn","massa_wg_perdida_mn" ,
          "composicao_massa_wg_mn", "discriminacao_massa_wg_mn", 
          "massa_emp_perdida_mn",
          "composicao_massa_emp_mn", "discriminacao_massa_emp_mn")
dt1 <- dt1[, (mult) := lapply(.SD, function(x) x * -1), .SDcols = mult]

# Tabela 1 - resultados perda da massa salarial
dt_homem_m <- dt1[, .(Ano_trimestre, total_hn, massa_wg_perdida_hn, 
                      composicao_massa_wg_hn, discriminacao_massa_wg_hn,
                      massa_emp_perdida_hn,
                      composicao_massa_emp_hn, discriminacao_massa_emp_hn)]


dt_mulher_m <- dt1[, .(Ano_trimestre, total_mn, massa_wg_perdida_mn, 
                       composicao_massa_wg_mn, discriminacao_massa_wg_mn,
                       massa_emp_perdida_mn,
                       composicao_massa_emp_mn, discriminacao_massa_emp_mn)]

dt_homem_m <- setnames(dt_homem_m, 
                       c("total_hn","massa_wg_perdida_hn" ,
                         "composicao_massa_wg_hn", "discriminacao_massa_wg_hn", 
                         "massa_emp_perdida_hn",
                         "composicao_massa_emp_hn", "discriminacao_massa_emp_hn"), 
                       c("Massa Salarial Total Perdida HN","Penalidade Salarial",
                         "Efeito Composição Salário HN","Efeito Discriminação Salário HN", 
                         " Penalidade Empregabilidade",
                         "Efeito Composição da Empregabilidade HN",
                         "Efeito Discriminação da Empregabilidade HN"))

dt_mulher_m <- setnames(dt_mulher_m,
                        c("total_mn","massa_wg_perdida_mn" ,
                          "composicao_massa_wg_mn", "discriminacao_massa_wg_mn", 
                          "massa_emp_perdida_mn",
                          "composicao_massa_emp_mn", "discriminacao_massa_emp_mn"), 
                        c("Massa Salarial Total Perdida MN","Penalidade Salarial",
                          "Efeito Composição Salário MN","Efeito Discriminação Salário MN", 
                          "Penalidade Empregabilidade",
                          "Efeito Composição da Empregabilidade MN",
                          "Efeito Discriminação da Empregabilidade MN"))

# Transpor cada data.table
# Para homens

homens_t <- setnames(dt_homem_m[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                     dt_homem_m[, c('variaveis', Ano_trimestre)])[]
setnames(homens_t, c("2023T1", "2024T1"), c("Homem_2023T1", "Homem_2024T1"))
homens_t$variaveis <- gsub("HN", "", homens_t$variaveis)

# Para mulheres
mulheres_t <- setnames(dt_mulher_m[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"Ano_trimestre"], 
                       dt_mulher_m[, c('m', Ano_trimestre)])[]
setnames(mulheres_t, c("2023T1", "2024T1"), c("Mulher_2023T1", "Mulher_2024T1"))

massa_recente <- cbind(homens_t, mulheres_t)
massa_recente <- massa_recente[, m := NULL]
massa_recente <- massa_recente[, Total_2023T1 := Homem_2023T1 + Mulher_2023T1]
massa_recente <- massa_recente[, Total_2024T1 := Homem_2024T1 + Mulher_2024T1]
massa_recente <- setcolorder(massa_recente, c("variaveis", "Homem_2023T1", "Mulher_2023T1",
                                              "Total_2023T1", "Homem_2024T1", "Mulher_2024T1", "Total_2024T1"))
massa_recente <- setnames(massa_recente, c("variaveis", "Homem_2023T1", "Mulher_2023T1",
                                           "Total_2023T1", "Homem_2024T1", "Mulher_2024T1", "Total_2024T1"),
                          c("Resultados", "Homem 2023T1", "Mulher 2023T1",
                            "Total 2023T1", "Homem 2024T1", "Mulher 2024T1", "Total 2024T1"))


table_massa <-
  kable(head(massa_recente, 7), align = 'l', booktabs = TRUE, 
        col.names = rep("", ncol(massa_recente)), format = 'latex') %>% 
  column_spec(1, width = "250px") %>%  
  column_spec(c(2:7), width = "60px") %>% 
  row_spec(1, bold = TRUE) %>% 
  row_spec(c(1:7), color = "black") %>% 
  add_header_above(c("", "Homem" = 1, "Mulher" = 1, "Total" = 1, "Homem" = 1, "Mulher" = 1, "Total" = 1)) %>% 
  add_header_above(c("", "2023" = 3, "2024" = 3)) %>% 
  add_indent(c(3, 4, 6, 7), level_of_indent = 2) %>% 
  row_spec(c(2, 5), italic = TRUE)

writeLines(table_massa, file.path(outputs, "table_massa.tex"))

