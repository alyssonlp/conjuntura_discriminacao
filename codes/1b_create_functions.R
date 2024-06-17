# Criando a função para calcular os rendimentos medios desagregados

 # data_base - é o data.frame usado
 # renda - se é renda habitual ou efetiva
 # peso - peso amostral
 # grupo - genero, raca, idade, ocupacao
 
 med_conj_desag_fun <- function(data_base, renda, peso, grupo) {
  library(data.table) 
  library(questionr)
   
   {{data_base}}[, list(avg_wage = wtd.mean(get(renda), weights = get(peso))),
      by = c({{grupo}})]
  
 }

# Criando a função para calcular o rendimento medio nacional
 med_conj_fun <- function(data_base, renda, peso) {
   library(data.table)
   library(questionr)
   {{data_base}}[, list(avg_wage = wtd.mean(get(renda), weights = get(peso)))]
 }

# criando a função para calcular a massa salarial
 
massa_salarial_fun <- function(data_base, salario_medio){
  library(data.table)
  trabalhadores <- data_base[VD4002 == 1, .(soma_peso = sum(V1028))]
  trabalhadores[, soma_peso := soma_peso * salario_medio]
   
}


# Criando função para calcular a variacao interanual


delta_interanual_fun <- function(data_base, categoria) {
  delta_interanual <- aggregate(avg_wage ~ categoria, data = as.data.frame(data_base), 
                                     FUN = function(x) (((x[2] - x[1]) / x[1]) * 100)) 
}
