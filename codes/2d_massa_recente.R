dt <- fread(file.path(csv_files, "resultados_massa_salarial.csv"))

# Tabela 1 - resultados perda da massa salarial
dt_homem_m <- dt[Ano_trimestre %in% c("2023T1", "2024T1"), 
                 .(Ano_trimestre,
                   massa_salarial_total_perdida_hn, massa_salarial_perdida_hn,
                   massa_salarial_composicao_hn, massa_salarial_discriminacao_hn,
                   massa_salarial_perdida_emp_hn, massa_salarial_composicao_emp_hn,
                   massa_salarial_discriminacao_emp_hn)]


dt_mulher_m <- dt[Ano_trimestre %in% c("2023T1", "2024T1"), 
                  .(Ano_trimestre,
                    massa_salarial_total_perdida_mn, massa_salarial_perdida_mn,
                    massa_salarial_composicao_mn, massa_salarial_discriminacao_mn,
                    massa_salarial_perdida_emp_mn, massa_salarial_composicao_emp_mn,
                    massa_salarial_discriminacao_emp_mn )]


dt_homem_m <- setnames(dt_homem_m, c("massa_salarial_total_perdida_hn", "massa_salarial_perdida_hn",
                     "massa_salarial_composicao_hn", "massa_salarial_discriminacao_hn",
                     "massa_salarial_perdida_emp_hn", "massa_salarial_composicao_emp_hn",
                     "massa_salarial_discriminacao_emp_hn"), 
                     c("Massa Salarial HN", "Efeito Total Salarial HN",
                      "Efeito Composição Salário HN", "Efeito Discriminação Salário HN",
                      "Efeito Total da Empregabilidade HN", "Efeito Composição da Empregabilidade HN",
                      "Efeito Discriminação da Empregabilidade HN" ))

dt_mulher_m <- setnames(dt_mulher_m, c ("massa_salarial_total_perdida_mn", "massa_salarial_perdida_mn",
                   "massa_salarial_composicao_mn", "massa_salarial_discriminacao_mn",
                   "massa_salarial_perdida_emp_mn", "massa_salarial_composicao_emp_mn",
                   "massa_salarial_discriminacao_emp_mn"),
                 c( "Massa Salarial MN", "Efeito Total Salarial MN",
                    "Efeito Composição Salário MN", "Efeito Discriminação Salário MN",
                    "Efeito Total da Empregabilidade MN", "Efeito Composição da Empregabilidade MN",
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

fwrite(massa_recente, file.path(csv_files, "resultados_recentes_massa.csv"))


# Tabela 2 - resultados perda de rendimento ou empregabilidade por pessoa
# Homens
dt_homem_e <- dt[Ano_trimestre %in% c("2023T1", "2024T1"), 
                 .(Ano_trimestre,
                   penalidade_salarial_hn, composicao_hn_wg, discriminacao_hn_wg,
                   penalidade_emp_hn, composicao_hn_emp, discriminacao_hn_emp)]
# Mulheres
dt_mulher_e <- dt[Ano_trimestre %in% c("2023T1", "2024T1"), 
                  .(Ano_trimestre,
                    penalidade_salarial_mn, composicao_mn_wg, discriminacao_mn_wg,
                    penalidade_emp_mn, composicao_mn_emp, discriminacao_mn_emp)]

# Renomeando as variaveis
dt_homem_e <-setnames(dt_homem_e, c("penalidade_salarial_hn", "composicao_hn_wg", "discriminacao_hn_wg",
                                    "penalidade_emp_hn", "composicao_hn_emp", "discriminacao_hn_emp"),
                      c("Penalidade Salarial HN", "E. Composição Salários HN", "E. Discriminação Salários HN",
                        "Penalidade na Empregabilidade HN", "E. Composição Emprego HN", "E. Discriminação Emprego HN"))
dt_mulher_e <-setnames(dt_mulher_e, c("penalidade_salarial_mn", "composicao_mn_wg", "discriminacao_mn_wg",
                                    "penalidade_emp_mn", "composicao_mn_emp", "discriminacao_mn_emp"),
                      c("Penalidade Salarial MN", "E. Composição Salários", "E. Discriminação Salários MN",
                        "Penalidade na Empregabilidade MN", "E. Composição Emprego MN", "E. Discriminação Emprego MN"))


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


fwrite(individual_recente, file.path(csv_files, "resultados_recentes_individual.csv"))
