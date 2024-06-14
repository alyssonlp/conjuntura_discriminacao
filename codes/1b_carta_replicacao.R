# Replicando a Carta de Conjuntura IPEA
ano <- c(2024)
trimestre <- c(1)

for(aa in ano) {
  for (tri in trimestre) {
    pnad_txt <- sprintf("PNADC_0%d%d.txt", tri, ano)
    dt <- read_pnadc(pnad_txt, "input_PNADC_trimestral.txt", 
                 vars = c( "UF", "RM_RIDE", "UPA", "Estrato", "V1008", "V1014",
                           "V1016","V1022", "V1023", "V1027","V1028", "V2005", 
                           "V2007",  "V2009", "V2010", "V4032","VD3004", 
                           "VD3005",  "VD4001", "VD4002", "VD4009", "VD4010",
                           "VD4016", "VD4017", "VD4019", "VD4020"))
    
    dt <- pnadc_deflator(dt, "deflator_PNADC_2024_trimestral_010203.xls")

is.data.table(dt) == TRUE
dt <-as.data.table(dt)

# Na carta a amostra consiste em individuos com idade igual ou superior a 14 anos
dt <- dt[V2009 >= 14]

# Regioes braileiras
dt[, region := floor(as.numeric(UF)/10)]

# Gênero
dt[, male := as.numeric(V2007 == 1)]

# Faixa etaria
dt[, age_group := 
     case_when(
       V2009 >= 14 & V2009 <= 24 ~ "14-24 anos",
       V2009 >= 25 & V2009 <= 39 ~ "25-39 anos",
       V2009 >= 40 & V2009 <= 59 ~ "40-59 anos",
       V2009 >= 60 ~ "60 anos ou mais"
     )
  ]

# Chefe de familia
dt[, chefe_familia := as.numeric(V2005 == 1)]

# Grau de escolaridade
dt[, VD3005 := as.numeric(VD3005)]
dt[,escolaridade :=
     case_when(
       VD3005 == 1 | VD3005 == 2 ~ "Fundamental Incompleto",
       VD3005 == 3 ~ "Fundamental Completo",
       VD3005 == 4 ~ "Médio Incompleto",
       VD3005 == 5 | VD3005 == 6 ~ "Médio Completo",
       VD3005 == 7 ~ "Superior Completo"
     )
  ]


# Morador de regiao metropolitana
dt[, metropolitan := as.numeric(V1023 == 1 | V1023 == 2)]

# Setor de atividade
dt[,VD4010 := as.numeric(VD4010)]
dt[, setor_atividade := 
     case_when(
       VD4010 == 1 ~ "Agricultura",
       VD4010 == 2 ~ "Industria",
       VD4010 == 3 ~ "Construção",
       VD4010 == 4 ~ "Comercio",
       VD4010 == 7 ~ "Servicos_profissionais",
       VD4010 == 5 ~ "Transporte",
       VD4010 == 10 | VD4010 == 11 ~ "Servicos_pessoais_coletivos",
       VD4010 == 8 ~ "Adm_publica",
       VD4010 == 9 ~ "Educ_saude",
       VD4010 == 6 ~ "Alojamento_alimentacao"
     )
  ]


# Vinculo empregaticio
dt[, VD4009 := as.numeric(VD4009)]
dt[, vinculo :=
     case_when(
       VD4009 == 1 ~ " Privado com carteira", 
       VD4009 == 2 ~ " Privado sem carteira", 
       VD4009 == 5 | VD4009 == 6 ~ "Setor Publico", 
       VD4009 == 9 ~ "Conta-propria"
     )
  ]

# renda habitual real - trabalho principal
dt[, r_hab := VD4016 * Habitual]

# renda efetiva real - trabalho principal
dt[, r_efe := VD4017 * Efetivo]

# renda habitual real - todos os trabalhos
dt[, r_hab_all := VD4019 * Habitual]

# renda efetiva real - todos os trabalhos
dt[, r_efe_all := VD4020 * Efetivo]

rds_file <- sprintf("pnadc%d_1_carta.rds", ano)
saveRDS(dt, file.path(intermediary_data, rds_file))

  }
}
