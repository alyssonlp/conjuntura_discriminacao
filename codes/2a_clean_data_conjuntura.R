ano = c(2024)
trimestre = c(1)

for(aa in ano) {
  for (tri in trimestre) {
    pnad_txt <- sprintf("PNADC_0%d%d.txt", tri, ano)
dt <- read_pnadc(pnad_txt, "input_PNADC_trimestral.txt", 
                 vars = c( "UF", "RM_RIDE", "UPA", "Estrato", "V1008", "V1014",
                           "V1016","V1022", "V1023", "V1027","V1028",  "V2005",
                           "V2007",  "V2009", "V2010", "V4032","VD3004", "VD3005" ,
                           "VD4001","VD4002", "VD4009", "VD4010", "VD4016", "VD4017",
                           "VD4019", "VD4020"))
dt <- pnadc_deflator(dt, "deflator_PNADC_2024_trimestral_010203.xls")

dt <-as.data.table(dt)

# Numero de filhos - do responsavel e/ou do conjuge
dt[, V2005 := as.numeric(V2005)]
dt[, child := as.numeric(V2005 == 4| V2005 == 5)]
dt[, match_child := max(child), by =.(UPA, Estrato, V1014, V1008, V1016)]
dt[, have_child := as.numeric(match_child == 1 & V2005 == 1 | V2005==2 | V2005 == 3)]

# Dummy gender
dt[, male := as.numeric(V2007 == 1)]

# maes
dt[, mother := as.numeric(male == 0 & have_child == 1)]


# Limitando a idade entre 25 e 54 anos
dt[,V2009 := as.numeric(V2009)]
setnames(dt, "V2009", "age")
dt <- dt[age >= 25 & age <= 54,]

# Dummy raca
# mantendo apenas brancos, pardos e pretos
dt[, V2010 := as.numeric (V2010)]
dt <- dt[V2010 %in% c(1, 2, 4),]
dt[, white := as.numeric(V2010 == 1)]
dt[, nonwhite := as.numeric(V2010 == 2 | V2010 == 4)]

# Criando variaiveis de controle - X
  # anos de estudo (quarta potencia)
  # experiencia potencial (idade - anos de estudo - 6)
  # genero interagido com o casamento
  # Dummies: UF, zona rural, metropolitana, ocupacao, setor economico, contrato de trabalho

# Anos de estudo
setnames(dt, "VD3005", "educ")
dt[, educ := as.numeric(educ)]
dt[, educ_sq := educ^2]
dt[, educ_cubic := educ^3]
dt[, educ_forth := educ^4]

# Experiencia potencial
dt[, potential_xp := age - educ - 6 ]

# Dummy married
dt[, conjuge := as.numeric(V2005 == 2)]
dt[, match_conjuge := max(conjuge), by =.(UPA, Estrato, V1014, V1008, V1016)]
dt[, married := as.numeric(match_conjuge == 1 & V2005 == 1)]

# married_woman
dt[, married_woman := as.numeric(male == 0 & married == 1)]

# Dummies UF
# Lista de estados e seus códigos
estados <- c("RO" = 11, "AC" = 12, "AM" = 13, "RR" = 14, "PA" = 15, "AP" = 16, "TO" = 17,
             "MA" = 21, "PI" = 22, "CE" = 23, "RN" = 24, "PB" = 25, "PE" = 26, "AL" = 27,
             "SE" = 28, "BA" = 29, "MG" = 31, "ES" = 32, "RJ" = 33, "SP" = 35, "PR" = 41,
             "SC" = 42, "RS" = 43, "MS" = 50, "MT" = 51, "GO" = 52, "DF" = 53)

# Loop para criar as colunas
for (estado in names(estados)) {
  dt[, (estado) := as.numeric(UF == estados[estado])]
}


# Dummy rural
dt[, rural := as.numeric(V1022 == 2)]

# Dummy metropolitana
dt[, metropolitan := as.numeric(V1023 == 1 | V1023 == 2)]

# Dummy ocupacao
dt[, emp := as.numeric(VD4002 == 1)]

# Dummy setor economico
dt[, VD4009 := as.numeric(VD4009)]
dt[, private_formal := as.numeric(VD4009 == 1)]
dt[, private_informal := as.numeric(VD4009 == 2)]
dt[, domestic_formal := as.numeric(VD4009 == 3)]
dt[, domestic_informal := as.numeric(VD4009 == 4)]
dt[, public_service := as.numeric(VD4009 >= 5 & VD4009 <=7)]
dt[, employer := as.numeric(VD4009 == 8)]
dt[, self_employed := as.numeric(VD4009 == 9)]
dt[, aux_familiar := as.numeric(VD4009 == 10)]

# Dummy contrato de trabalho - formal ou informal
dt[, formal := as.numeric(V4032 == 1)]

# Setor de atividade
dt[,VD4010 := as.numeric(VD4010)]
dt[, agricultura := as.numeric(VD4010 == 1)]
dt[, industria := as.numeric(VD4010 == 2)]
dt[, construcao := as.numeric(VD4010 == 3)]
dt[, comercio := as.numeric(VD4010 == 4)]
dt[, servios_profissionais := as.numeric(VD4010 == 7)]
dt[, transporte := as.numeric(VD4010 == 5)]
dt[, servicos_pessoais_coletivos := as.numeric(VD4010 == 10 | VD4010 == 11)]
dt[, adm_publica := as.numeric(VD4010 == 8)]   
dt[, educ_saude := as.numeric(VD4010 == 9)]
dt[, alojamento_alimentacao := as.numeric (VD4010 == 6)]

# renda habitual real - trabalho principal
dt[, r_hab := VD4016 * Habitual]

# renda efetiva real - trabalho principal
dt[, r_efe := VD4017 * Efetivo]

# renda habitual real - todos os trabalhos
dt[, r_hab_all := VD4019 * Habitual]

# renda efetiva real - todos os trabalhos
dt[, r_efe_all := VD4020 * Efetivo]

# ln da renda
dt[, ln_r_hab := log(r_hab)]
dt[, ln_r_efe := log(r_efe)]
dt[, ln_r_hab_all := log(r_hab_all)]
dt[, ln_r_efe_all := log(r_efe_all)]


rds_file <- sprintf("pnadc%d_%d.rds", aa, tri)
saveRDS(dt, file.path(intermediary_data, rds_file))

  }
}


