# Criando a função para calcular os rendimentos medios

 # data_base - é o data.frame usado
 # renda - se é renda habitual ou efetiva
 # peso - peso amostral
 # grupo - genero, raca, idade, ocupacao
 
 media_conjuntura <- function(data_base, renda, peso, grupo) {
  library(data.table) 
  library(questionr)
   
   {{data_base}}[, list(avg_wage = wtd.mean(get(renda), weights = get(peso))),
      by = c({{grupo}})]
  
 }

 

 


 
