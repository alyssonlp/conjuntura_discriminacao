# Script para gerar os resultados de rendimentos e massa salarial
dt <- readRDS(file.path(intermediary_data, "pnadc2024_1_carta.rds"))
source("C:/Users/Rodrigo/Documents/R/NERI_INSPER/conjuntura/codes/1c_create_functions.R")

# Rendimento habitual medio - toda populacao
rendimento_hab_medio <- media_conjuntura(dt,"VD4016", "V1028", "")

# Rendimento médio habitual - por tipo de vinculo
vinculo_hab <- media_conjuntura(dt,"VD4016", "V1028", "vinculo")


# Rendimento médio efetivo - por tipo de vinculo
vinculo_efe <- media_conjuntura(dt,"VD4017", "V1028", "vinculo")


# Rendimento medio habitual real por dados desagregados
#Regioes
regioes <- media_conjuntura(dt,"VD4016", "V1028", "region")

# Genero
genero <- media_conjuntura(dt,"VD4016", "V1028", "male")
genero[male == 0, Genero := "Mulher"]
genero[male == 1, Genero := "Homem"]

# Faixa etaria
age_group <- media_conjuntura(dt,"VD4016", "V1028", "age_group")

# Escolaridade
escolaridade <- media_conjuntura(dt,"VD4016", "V1028", "escolaridade")

# Chefe de familia
chefe_familia <- media_conjuntura(dt,"VD4016", "V1028", "chefe_familia")

# Regiao metropolitana
metropolitana <- media_conjuntura(dt,"VD4016", "V1028", "metropolitan")

# Setor de atividade
setor_atividade <- media_conjuntura(dt,"VD4016", "V1028", "setor_atividade")
