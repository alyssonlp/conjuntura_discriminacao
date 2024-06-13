
df <- read_pnadc("PNADC_012024.txt", "input_PNADC_trimestral.txt", 
                 vars = c( "UF", "RM_RIDE", "UPA", "Estrato", "V1008", "V1014",
                           "V1016","V1022", "V1027","V1028",  "V2005",
                           "V2007",  "V2009", "V2010", "V4032","VD3004", "VD4001",
                           "VD4002", "VD4009", "VD4010", "VD4016"))

is.data.table(df) == TRUE
df <-as.data.table(df)

# Numero de filhos - do responsavel e/ou do conjuge
df[, V2005 := as.numeric(V2005)]
df[, child := as.numeric(V2005 == 4| V2005 == 5)]
df[, match_child := max(child), by =.(UPA, Estrato, V1014, V1008, V1016)]
df[, have_child := as.numeric(match_child == 1 & V2005 == 1)]

# Dummy gender
df[, male := as.numeric(V2007 == 1)]

# maes
df[, mother := as.numeric(male == 0 & have_child == 1)]


# Limitando a idade entre 25 e 54 anos
df[,V2009 := as.numeric(V2009)]
setnames(df, "V2009", "age")
df <- df[age >= 25 & age <= 54,]

# Dummy raca
# mantendo apenas brancos, pardos e pretos
df[, V2010 := as.numeric (V2010)]
df <- df[V2010 %in% c(1, 2, 4),]
df[, white := as.numeric(V2010 == 1)]
df[, nonwhite := as.numeric(V2010 == 2 | V2010 == 4)]

# Criando variaiveis de controle - X
  # anos de estudo (quarta potencia)
  # experiencia potencial (idade - anos de estudo - 6)
  # genero interagido com o casamento
  # Dummies: UF, zona rural, metropolitana, ocupacao, setor economico, contrato de trabalho

# Anos de estudo
setnames(df, "VD3004", "educ")
df[, educ := as.numeric(educ)]
df[, educ_sq := educ^2]
df[, educ_cubic := educ^3]
df[, educ_forth := educ^4]

# Experiencia potencial
df[, potential_xp := age - educ - 6 ]

# Dummy married
df[, conjuge := as.numeric(V2005 == 2)]
df[, match_conjuge := max(conjuge), by =.(UPA, Estrato, V1014, V1008, V1016)]
df[, married := as.numeric(match_conjuge == 1 & V2005 == 1)]

# married_woman
df[, married_woman := as.numeric(male == 0 & married == 1)]

# Dummies UF
# Lista de estados e seus códigos
estados <- c("RO" = 11, "AC" = 12, "AM" = 13, "RR" = 14, "PA" = 15, "AP" = 16, "TO" = 17,
             "MA" = 21, "PI" = 22, "CE" = 23, "RN" = 24, "PB" = 25, "PE" = 26, "AL" = 27,
             "SE" = 28, "BA" = 29, "MG" = 31, "ES" = 32, "RJ" = 33, "SP" = 35, "PR" = 41,
             "SC" = 42, "RS" = 43, "MS" = 50, "MT" = 51, "GO" = 52, "DF" = 53)

# Loop para criar as colunas
for (estado in names(estados)) {
  df[, (estado) := as.numeric(UF == estados[estado])]
}


# Dummy rural
df[, rural := as.numeric(V1022 == 2)]

# Dummy ocupacao
df[, unemp := as.numeric(VD4002 == 2)]

# Dummy setor economico
df[, private_formal := as.numeric(VD4009 == 1)]
df[, private_informal := as.numeric(VD4009 == 2)]
df[, domestic_formal := as.numeric(VD4009 == 3)]
df[, domestic_informal := as.numeric(VD4009 == 4)]
df[, public_service := as.numeric(VD4009 >= 5 & VD4009 <=7)]
df[, employer := as.numeric(VD4009 == 8)]
df[, self_employed := as.numeric(VD4009 == 9)]
df[, aux_familiar := as.numeric(VD4009 == 10)]

# Dummy contrato de trabalho - formal ou informal
df[, formal := as.numeric(V4032 == 1)]

saveRDS(df, file.path(intermediary_data, "pnadc2024_1.rds"))

