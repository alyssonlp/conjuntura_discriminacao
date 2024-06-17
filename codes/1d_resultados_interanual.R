# Arquivo com as funcoes a serem usadas
source("C:/Users/Rodrigo/Documents/R/NERI_INSPER/conjuntura/codes/1b_create_functions.R")

# Importando resultados de 1d_resultados_carta.R 
ano_0 <- c(2023)
ano_1 <- c(2024)
trimestre <- c(1)
resultados_media <- c("age_group", "chefe_familia", "escolaridade", "male",
                      "massa_salarial_real", "metropolitan", "nacional", "region",
                      "setor_atividade", "vinculo", "vinculo_efetivo")

lista_ano_0 <- list()
lista_ano_1 <- list()

for(a0 in ano_0) {
  for(a1 in ano_1) {
    for (tri in trimestre) {
      for (nome in resultados_media) {
        nome_csv_0 <- sprintf("resultado_%s_%d_%d.csv", nome, a0, tri)
        lista_ano_0[[nome_csv_0]] <- read.csv(file.path(csv_files, nome_csv_0))
        nome_csv_1 <- sprintf("resultado_%s_%d_%d.csv", nome, a1, tri)
        lista_ano_1[[nome_csv_1]] <- read.csv(file.path(csv_files, nome_csv_1))
      }
    }
  }
}


# Transformando os resultados de list em diversos data.table


for (dt_0 in names(lista_ano_0)) {
  name_dt <- gsub("\\.csv", "", dt_0)
  assign(paste0("dt_", name_dt),
         as.data.table(lista_ano_0[[dt_0]]))
}

for (dt_1 in names(lista_ano_1)) {
  name_dt <- gsub("\\.csv", "", dt_1)
  assign(paste0("dt_", name_dt),
         as.data.table(lista_ano_1[[dt_1]]))
}


# Criando lista com os rbind pareados 
lista_dt_pair <- list()

# Nomes dos data.table
dt_nome <- c()

for(a0 in ano_0) {
  for(a1 in ano_1) {
    for (tri in trimestre) {
      for (nome in resultados_media) {
        nome_dt_pair <- paste0("dt_resultado_", nome)
        pair_0 <- sprintf("dt_resultado_%s_%d_%d", nome, a0, tri)
        pair_1 <- sprintf("dt_resultado_%s_%d_%d", nome, a1, tri)
        dt_pair <- rbind(get(pair_0), get(pair_1))
        lista_dt_pair[[nome_dt_pair]] <- dt_pair
      }
    }
  }
}
        

for (nn in names(lista_dt_pair)) {
  assign(paste0("pair_", nn),
         as.data.table(lista_dt_pair[[nn]]))
}


# criando a variavel Brasil em pair_dt_resultado_nacional
pair_dt_resultado_nacional[, Brasil := as.numeric(1)]
# criando a variavel massa_salarial em pair_dt_resultado_massa_salarial_real
pair_dt_resultado_massa_salarial_real[, massa_salarial := as.numeric(1)]

  

for (names in resultados_media) {
  ns <- sprintf("pair_dt_resultado_%s" , names)
  interanual <- c(interanual, ns)
}
interanual

setnames(pair_dt_resultado_vinculo_efetivo, "vinculo", "vinculo_efe")


categoria <- c("age_group", "chefe_familia", "escolaridade", "male",
              "soma_peso", "metropolitan", "region",
              "setor_atividade", "vinculo", "vinculo_efe", "Brasil", 
              "massa_salarial")


# estou travado nesse looping
for (pair_dt in lista_dt_pair) {
  for (cat in categoria) {
    dt <- get(pair_dt)
   resultado_interanual <- delta_interanual_fun(dt, dt$cat)
   file_name <- paste0("resultado_", pair_dt, "_", cat, ".csv")
   write.csv(file.path(csv_files, resultado_interanual, file_name, row.names = FALSE))
  }
}


## testando o funcionamento dos codigos
abc <- delta_interanual_fun(pair_dt_resultado_age_group, pair_dt_resultado_age_group$age_group)

aab <- aggregate(avg_wage ~ age_group, data = as.data.frame(pair_dt_resultado_age_group), 
          FUN = function(x) (((x[2] - x[1]) / x[1]) * 100)) 

for (pair_dt in pair_dt_resultado_age_group) {
    dt <- get(pair_dt)
    aaa <- delta_interanual_fun(dt, dt$cat)
    
}

### abc e aab funcionam, o looping não está funcionando

# Resultados interanual

inter_age_group <- delta_interanual_fun(pair_dt_resultado_age_group, 
                                        pair_dt_resultado_age_group$age_group)

inter_chefe_familia <- delta_interanual_fun(pair_dt_resultado_chefe_familia, 
                                            pair_dt_resultado_chefe_familia$chefe_familia)


inter_escolaridade <- delta_interanual_fun(pair_dt_resultado_escolaridade, 
                                           pair_dt_resultado_escolaridade$escolaridade)


inter_male <- delta_interanual_fun(pair_dt_resultado_male, 
                                   pair_dt_resultado_male$male)

setnames(pair_dt_resultado_massa_salarial_real, "soma_peso", "avg_wage")
inter_massa <- delta_interanual_fun(pair_dt_resultado_massa_salarial_real, 
                                    pair_dt_resultado_massa_salarial_real$massa_salarial)


inter_metropolitan <- delta_interanual_fun(pair_dt_resultado_metropolitan, 
                                          pair_dt_resultado_metropolitan$metropolitan)


inter_region <- delta_interanual_fun(pair_dt_resultado_region, 
                                     pair_dt_resultado_region$region)


inter_setor_atividade <- delta_interanual_fun(pair_dt_resultado_setor_atividade, 
                                              pair_dt_resultado_setor_atividade$setor_atividade)


inter_vinculo <- delta_interanual_fun(pair_dt_resultado_vinculo, 
                                      pair_dt_resultado_vinculo$vinculo)


inter_vinculo_efe <- delta_interanual_fun(pair_dt_resultado_vinculo_efetivo, 
                                          pair_dt_resultado_vinculo_efetivo$vinculo_efe)


inter_Brasil <- delta_interanual_fun(pair_dt_resultado_nacional, 
                                     pair_dt_resultado_nacional$Brasil)


resultado_tables <- ls(pattern = "^inter_")
resultado_tables

for (rr in resultado_tables) {
  for(a0 in ano_0) {
    for(a1 in ano_1) {
      for (tri in trimestre) {
  file_name <- sprintf("%s_%d_%d_%d.csv", rr, a0, a1, tri)
  write.csv(get(rr),
            file.path(csv_files, file_name), row.names = FALSE)
      }
    }
  }
}
    
