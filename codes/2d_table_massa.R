dt <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))


dt1 <- dt[Ano_trimestre %in% c("2023T1", "2024T1"),]
dt1[, total_perdido_hn := massa_wg_perdida_hn + massa_emp_perdida_hn]
dt1[, total_perdido_mn := massa_wg_perdida_mn + massa_emp_perdida_mn ]

mult <- c("total_perdido_hn","massa_wg_perdida_hn" ,
          "massa_wg_composicao_hn", "massa_wg_discriminacao_hn", 
          "massa_emp_perdida_hn",
          "massa_emp_composicao_hn", "massa_emp_discriminacao_hn",
          "total_perdido_mn","massa_wg_perdida_mn" ,
          "massa_wg_composicao_mn", "massa_wg_discriminacao_mn", 
          "massa_emp_perdida_mn",
          "massa_emp_composicao_mn", "massa_emp_discriminacao_mn")
dt1 <- dt1[, (mult) := lapply(.SD, function(x) x * -1), .SDcols = mult]

# Tabela 1 - resultados perda da massa salarial
dt_homem_m <- dt1[, .(Ano_trimestre, total_perdido_hn, massa_wg_perdida_hn, 
                      massa_wg_composicao_hn, massa_wg_discriminacao_hn,
                      massa_emp_perdida_hn,
                      massa_emp_composicao_hn, massa_emp_discriminacao_hn)]


dt_mulher_m <- dt1[, .(Ano_trimestre, total_perdido_mn, massa_wg_perdida_mn, 
                       massa_wg_composicao_mn, massa_wg_discriminacao_mn,
                       massa_emp_perdida_mn,
                       massa_emp_composicao_mn, massa_emp_discriminacao_mn)]

dt_homem_m <- setnames(dt_homem_m, 
                       c("total_perdido_hn","massa_wg_perdida_hn" ,
                         "massa_wg_composicao_hn", "massa_wg_discriminacao_hn", 
                         "massa_emp_perdida_hn",
                         "massa_emp_composicao_hn", "massa_emp_discriminacao_hn"), 
                       c("Massa Salarial Total Perdida HN","Gap Salarial",
                         "Efeito Composição Salário HN","Efeito Discriminação Salário HN", 
                         " Gap Empregabilidade",
                         "Efeito Composição da Empregabilidade HN",
                         "Efeito Discriminação da Empregabilidade HN"))

dt_mulher_m <- setnames(dt_mulher_m,
                        c("total_perdido_mn","massa_wg_perdida_mn" ,
                        "massa_wg_composicao_mn", "massa_wg_discriminacao_mn", 
                        "massa_emp_perdida_mn",
                        "massa_emp_composicao_mn", "massa_emp_discriminacao_mn"), 
                        c("Massa Salarial Total Perdida MN","Gap Salarial",
                          "Efeito Composição Salário MN","Efeito Discriminação Salário MN", 
                          "Gap Empregabilidade",
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
   kable(head(massa_recente, 7), align = 'l', booktabs = TRUE, col.names = rep("", ncol(massa_recente))) %>% 
  column_spec(1, width = "300px") %>%  
  column_spec(c(2:7), width = "100px") %>% 
  row_spec(1, bold = FALSE, background = "lightgray") %>% 
  add_header_above(c("", "Homem" = 1, "Mulher" = 1, "Total" = 1, "Homem" = 1, "Mulher" = 1, "Total" = 1)) %>% 
  add_header_above(c("", "2023" = 3, "2024" = 3)) %>% 
  pack_rows("Devido às diferenças salariais", 2, 4) %>% 
  pack_rows("Devido às diferenças na empregabilidade", 5, 7) %>% 
  add_indent(c(3, 4, 6, 7), level_of_indent = 3)

 save_kable(table_massa, file.path(tables_output, file = "table_massa.html"))
 webshot(file.path(tables_output,"table_massa.html"), file.path(tables_output, "table_massa.pdf"))

