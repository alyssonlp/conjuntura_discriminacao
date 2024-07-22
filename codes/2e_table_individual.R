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

dt_homem_e <- setnames(dt_homem_e, 
                       c("composicao_hn_wg", "discriminacao_hn_wg","composicao_hn_emp", 
                         "discriminacao_hn_emp", "penalidade_salarial_hn"), 
                       c( "Efeito Composição Salário HN","Efeito Discriminação Salário HN", "Efeito Composição da Empregabilidade HN",
                         "Efeito Discriminação da Empregabilidade HN", "Penalidade Salarial HN" ))

dt_mulher_e <- setnames(dt_mulher_e, 
                        c("composicao_mn_wg", "discriminacao_mn_wg","composicao_mn_emp", 
                          "discriminacao_mn_emp", "penalidade_salarial_mn"), 
                        c("Efeito Composição Salário MN","Efeito Discriminação Salário MN", "Efeito Composição da Empregabilidade MN",
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
individual_recente <- individual_recente[, Total_2023T1 := Homem_2023T1 + Mulher_2023T1]
individual_recente <- individual_recente[, Total_2024T1 := Homem_2024T1 + Mulher_2024T1]
individual_recente <- setcolorder(individual_recente,c("variaveis", "Homem_2023T1", "Mulher_2023T1",
                                                       "Total_2023T1", "Homem_2024T1", "Mulher_2024T1", "Total_2024T1"))
individual_recente <- setnames(individual_recente, c("variaveis", "Homem_2023T1", "Mulher_2023T1",
                                                     "Total_2023T1", "Homem_2024T1", "Mulher_2024T1", "Total_2024T1"),
                               c("Resultados", "Homem 2023T1", "Mulher 2023T1",
                                 "Total 2023T1", "Homem 2024T1", "Mulher 2024T1", "Total 2024T1"))


# Criando a tabela 
gt_ind <- individual_recente %>% 
  gt() %>% 
  cols_align(align = "right", columns = c(1:5)) %>% 
  tab_row_group(label = "Total", rows = 5) %>% 
  tab_row_group(label = "Decomposição", rows = 1:4) %>% 
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
  gtsave(file.path(tables_output, "table_individual.html"))
