
# Esse script gera e salva os resultados descritivos para a geração de graficos
# de acordo com 3 categorias: Brasil, raça, raça e genero
# Para gerar os resultados, utiliza-se a função criada em 0a

ano <- c(2012:2024)
trimestre <- c(1:4)

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=2){
      next  
    }
    
    # A função criada em 0a é utilizada nesse momento
    carta <- ano_tri_fun(aa, tri) 
    
    resultados_brasil <- sprintf("resultados_brasil_%d_%d.csv", aa, tri)
    carta_brasil <- write.csv(carta$resultados_br, file.path(temp_file, resultados_brasil))
    
    resultados_raca <- sprintf("resultados_raca_%d_%d.csv", aa, tri)
    carta_raca <- write.csv(carta$resultados_raca, file.path(temp_file, resultados_raca))
    
    resultados_gen_raca <- sprintf("resultados_genero_raca_%d_%d.csv", aa, tri)
    carta_gen_raca <- write.csv(carta$resultados_gen_raca, file.path(temp_file, resultados_gen_raca))
  }
}

# juntar os arquivos e criar csv: resultados_brasil, resultados_raca, resultados_genero_raca
# O computador nao suportou rodar o looping anterior para mais de um ano.

# Alterando o diretório de trabalho
setwd(file.path(temp_file)) 

arquivos <- list.files(pattern = "*.csv")

# Separar os arquivos por resultados
arquivos_brasil <- grep("resultados_brasil_", arquivos, value = TRUE)
arquivos_raca <- grep("resultados_raca_", arquivos, value = TRUE)
arquivos_genero_raca <- grep("resultados_genero_raca_", arquivos, value = TRUE)



# Criando a funcao "combinar_arquivos" para juntar csv conforme a categoria do resultado
combinar_arquivos_fun <- function(lista_arquivos) {
  lista_dados <- lapply(lista_arquivos, function(arquivo) {
    info <- unlist(strsplit(arquivo, "_"))
    
    if (grepl("resultados_genero_raca_", arquivo)) {
      ano <- as.numeric(info[4])
      trimestre <- as.numeric(gsub(".csv", "", info[5]))
    } else {
      ano <- as.numeric(info[3])
      trimestre <- as.numeric(gsub(".csv", "", info[4]))
    }
    
    dados <- read.csv(arquivo)
    dados$Ano <- ano
    dados$Trimestre <- trimestre
    dados$Ano_trimestre <- paste0(ano, "T", trimestre)
    
    # Removendo as colunas Ano, Trimestre e X, se existirem
    dados <- dados[ , !(names(dados) %in% c("Ano", "Trimestre", "X"))]
    
    return(dados)
  })
  
  # Juntando todos os dataframes em um único dataframe
  dados_join <- do.call(rbind, lista_dados)
  
  # Removendo linhas com NAs na coluna Ano_trimestre
  dados_join <- na.omit(dados_join)
  
  dados_join <- dados_join[dados_join$Ano_trimestre != "NATNA", ]
  
  return(dados_join)
}

# Utilizando a funcao combinar_arquivos_fun para juntá-los por tipo de resultados
dados_brasil <- combinar_arquivos_fun(arquivos_brasil)
dados_brasil$gender_race <- c("Brasil")
dados_raca <- combinar_arquivos_fun(arquivos_raca)
dados_genero_raca <- combinar_arquivos_fun(arquivos_genero_raca)

# Salvando em csv
setwd(file.path(csv_output)) 
write.csv(dados_brasil, "resultados_brasil_carta.csv", row.names = FALSE)
write.csv(dados_raca, "resultados_raca_carta.csv", row.names = FALSE)
write.csv(dados_genero_raca, "resultados_genero_raca_carta.csv", row.names = FALSE)

