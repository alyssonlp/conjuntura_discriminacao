# Criando a função para calcular os rendimentos medios desagregados

 # data_base - é o data.frame usado
 # renda - se é renda habitual ou efetiva
 # peso - peso amostral
 # grupo - genero, raca, idade, ocupacao
 
 med_conj_desag_fun <- function(data_base, renda, peso, grupo) {
   {{data_base}}[, list(avg_wage = wtd.mean(get(renda), weights = get(peso))),
      by = c({{grupo}})]
  
 }

# Criando a função para calcular o rendimento medio nacional
 med_conj_fun <- function(data_base, renda, peso) {
   {{data_base}}[, list(avg_wage = wtd.mean(get(renda), weights = get(peso)))]
 }

# criando a função para calcular a massa salarial
 
massa_salarial_fun <- function(data_base, salario_medio){
  trabalhadores <- data_base[VD4002 == 1, .(soma_peso = sum(V1028))]
  trabalhadores[, soma_peso := soma_peso * salario_medio]
   
}


# Criando função para calcular a variacao interanual


delta_interanual_fun <- function(data_base, categoria) {
  delta_interanual <- aggregate(avg_wage ~ categoria, data = as.data.frame(data_base), 
                                     FUN = function(x) (((x[2] - x[1]) / x[1]) * 100)) 
}


# calculando a media das variaveis
med_var_fun <- function(data_base, variavel, peso) {
  med_var <- data_base[, list(media_var = wtd.mean(get(variavel), weights = get(peso)))]
}

# Criando a funcao para ler e agregar dados por grupo

agreg_fun <- function(grupo, anos, trimestres, ano_24, tri_24) {
  lista <- list()
  for (aa in anos) {
    for (tri in trimestres) {
      nome_csv <- sprintf("conjuntura_%s_%d_%d.csv", grupo, aa, tri)
      caminho_arquivo <- file.path(csv_files, nome_csv)
      if (file.exists(caminho_arquivo)) {
        dados <- read.csv(caminho_arquivo)
        lista[[nome_csv]] <- dados
      }
    }
  }
  for (tr in tri_24) {
    nome_csv_24 <- sprintf("conjuntura_%s_%d_%d.csv", grupo, ano_24, tr)
    caminho_arquivo <- file.path(csv_files, nome_csv_24)
    if (file.exists(caminho_arquivo)) {
      dados <- read.csv(caminho_arquivo)
      lista[[nome_csv_24]] <- dados
    }
  }
  return(do.call(rbind, lista))
}