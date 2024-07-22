dt <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))
dt1 <- dt[Ano_trimestre %in% c("2023T1", "2024T1"),]
mult <- c("total_perdido_hn", "massa_wg_composicao_hn",
          "massa_wg_discriminacao_hn", "massa_emp_composicao_hn", "massa_emp_discriminacao_hn",
          "total_perdido_mn", "massa_wg_composicao_mn","massa_wg_discriminacao_mn",
          "massa_emp_composicao_mn", "massa_emp_discriminacao_mn")
dt1 <- dt1[, (mult) := lapply(.SD, function(x) x * -1), .SDcols = mult]

# Tabela 1 - resultados perda da massa salarial
dt_homem_m <- dt1[, .(Ano_trimestre, total_perdido_hn, massa_wg_composicao_hn, 
                      massa_wg_discriminacao_hn,
                      massa_emp_composicao_hn, massa_emp_discriminacao_hn)]


dt_mulher_m <- dt1[, .(Ano_trimestre,total_perdido_mn,massa_wg_composicao_mn, 
                       massa_wg_discriminacao_mn,
                       massa_emp_composicao_mn, massa_emp_discriminacao_mn)]

dt_homem_m <- setnames(dt_homem_m, 
                       c("total_perdido_hn", "massa_wg_composicao_hn", "massa_wg_discriminacao_hn",
                         "massa_emp_composicao_hn", "massa_emp_discriminacao_hn"), 
                       c("Massa Salarial Total Perdida HN", "Efeito Composição Salário HN",
                         "Efeito Discriminação Salário HN", "Efeito Composição da Empregabilidade HN",
                         "Efeito Discriminação da Empregabilidade HN" ))

dt_mulher_m <- setnames(dt_mulher_m, 
                       c("total_perdido_mn", "massa_wg_composicao_mn", "massa_wg_discriminacao_mn",
                         "massa_emp_composicao_mn", "massa_emp_discriminacao_mn"), 
                       c("Massa Salarial Total Perdida MN", "Efeito Composição Salário MN",
                         "Efeito Discriminação Salário MN", "Efeito Composição da Empregabilidade MN",
                         "Efeito Discriminação da Empregabilidade MN" ))

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

# Criando a tabela 
gt_massa <- massa_recente %>% 
  gt() %>% 
  cols_align(align = "right", columns = c(1:5)) %>% 
  tab_row_group(label = "Total", rows = 1) %>% 
  tab_row_group(label = "Decomposição", rows = 2:5) %>% 
  tab_spanner(label = "2024", columns = c("Homem 2024T1", "Mulher 2024T1", "Total 2024T1")) %>% 
  tab_spanner(label = "2023", columns = c("Homem 2023T1", "Mulher 2023T1", "Total 2023T1")) %>% 
  cols_label("Homem 2023T1" = "Homem", "Mulher 2023T1" = "Mulher", "Total 2023T1" = "Total",
             "Homem 2024T1" = "Homem", "Mulher 2024T1" = "Mulher", "Total 2024T1" = "Total",
             "Resultados" = "") %>% 
  tab_style( style = list( cell_text(weight = "bold")),
    locations = cells_row_groups(groups = "Total")) %>% 
  tab_style(style = list(cell_text(weight = "bold")),
    locations = cells_row_groups(groups = "Decomposição")) %>% 
  opt_stylize(style = 6, color = "gray", add_row_striping = FALSE) %>% 
  gtsave(file.path(tables_output, filename = "massa_table.html"))
