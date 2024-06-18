# Script para gerar os resultados de rendimentos e massa salarial

# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2024)
trimestre <- c(1)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    rds_file <- sprintf("pnadc%d_%d.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
    # Rendimento medio habitual do trabalho - nacional
    resultado_nacional <- med_conj_fun(dt, "r_hab_all", "V1028")
    resultado_nacional[, Brasil := 1]
    
    # Massa salarial
    massa <- massa_salarial_fun(dt, resultado_nacional$avg_wage)
    setDT(massa)
    massa[, massa_salarial := 1]
  
    
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
    
    
    # Transformando os resultados de list em diversos data.table
    
    for (dd in names(rendimento_medio_hab_desagregado)) {
      assign(paste0("resultado_", dd),
             as.data.table(rendimento_medio_hab_desagregado[[dd]]))
    }
    
    # Adicionar a variavel ano - importante para o calculo do crescimento interanual
    # Encontrar os data.frame que começa com "resultado_"
    resultado_tables <- ls(pattern = "^resultado_")
    resultado_tables
    
    
    # Loop sobre cada data.table encontrado
    for (rr in resultado_tables) {
      assign(rr, `[.data.table`(get(rr), , ano := aa))
    }
    
    
    # salvar os resultados do rendimento habitual medio em todos os trabalhos em csv
    
    for (rr in resultado_tables) {
      file_name <- sprintf("conjuntura_%s_%d_%d_habitual.csv", rr, aa, tri)
      write.csv(get(rr),
                file.path(csv_files, file_name), row.names = FALSE)
    }
    
    
    # salvar o resultado da massa salarial
    file_massa <- sprintf("conjuntura_massa_salarial_real_%d_%d.csv", aa, tri)
    massa[, ano := aa]
    write.csv(massa, file.path(csv_files, file_massa), row.names = FALSE)
    
  }
}
