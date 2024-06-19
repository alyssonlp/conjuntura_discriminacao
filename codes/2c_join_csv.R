# Definindo os anos e trimestres
anos <- c(2022, 2023)
trimestre <- 1:4
a_24 <- 2024
tri_24 <- 1

# Lista de grupos
grupos <- c("massa_salarial_real", "resultado_male", "resultado_mother", 
            "resultado_white", "resultado_nonwhite", "resultado_married_woman",
            "resultado_rural", "resultado_metropolitan", "resultado_private_formal", 
            "resultado_private_informal", "resultado_domestic_informal",
            "resultado_public_service", "resultado_employer", "resultado_self_employed",
            "resultado_aux_familiar", "resultado_agricultura", "resultado_industria",
            "resultado_construcao", "resultado_comercio", "resultado_servios_profissionais",
            "resultado_transporte", "resultado_servicos_pessoais_coletivos", "resultado_adm_publica", 
            "resultado_educ_saude", "resultado_alojamento_alimentacao", "resultado_nacional")



# Juntando em um só csv
for (grupo in grupos) {
  dados_grupo <- agreg_fun(grupo, anos, trimestre, a_24, tri_24)
  nome_arquivo_saida <- sprintf("resultado_%s.csv", grupo)
  write.csv(dados_grupo, file.path(csv_files, file = nome_arquivo_saida), row.names = FALSE)
}