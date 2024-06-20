# Script para gerar os resultados de rendimentos e massa salarial

# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2022, 2023, 2024)
trimestre <- c(1:4)


resultados_finais <- list()

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=2){
      next  
    }
    rds_file <- sprintf("pnadc%d_%d.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
    # Rendimento medio habitual do trabalho - nacional
    resultado_nacional <- med_conj_fun(dt, "r_hab_all", "V1028")
    resultado_nacional[, Brasil := 1]
    
    # Massa salarial
    resultado_massa <- massa_salarial_fun(dt, resultado_nacional$avg_wage)
    setDT(resultado_massa)
    resultado_massa[, massa_salarial := 1]
  
    
    # Como os outros resultados são a partir do rendimento habitual, tem-se:
    # Criacao do vetor de dados desagregados
    
    desagregados <- c("male", "mother", "white", "nonwhite", "married_woman",
                      "rural", "metropolitan" ,"private_formal", "private_informal",
                      "domestic_informal", "public_service", "employer",
                      "self_employed", "aux_familiar",  "agricultura", "industria",
                       "construcao", "comercio", "servios_profissionais", "transporte",
                        "servicos_pessoais_coletivos", "adm_publica", "educ_saude",
                      "alojamento_alimentacao")
    
    rendimento_medio_hab_desagregado <- list()
    
    for (dd in desagregados) {
      rendimento_medio_hab_desagregado[[dd]] <-
        med_conj_desag_fun(dt, "r_hab_all", "V1028", dd)
    }
    
    
    # Resultados por desagregacao
    for (dd in names(rendimento_medio_hab_desagregado)) {
      resultado <- as.data.table(rendimento_medio_hab_desagregado[[dd]])
      resultado[, ano := aa]  
      resultado[, trimestre := tri]  
      
    
      if (is.null(resultados_finais[[dd]])) {
        resultados_finais[[dd]] <- resultado
      } else {
        resultados_finais[[dd]] <- rbindlist(list(resultados_finais[[dd]], resultado), fill = TRUE)
      }
    }
    
  }
}

# Salvar resultados finais em arquivos CSV

  for (dd in names(resultados_finais)) {
  # Nome do arquivo de saída
  nome_arquivo_saida <- sprintf("%s.csv", dd)
  
  # Escrever o arquivo CSV
  write.csv(resultados_finais[[dd]], file.path(csv_files, nome_arquivo_saida), row.names = FALSE)
  }