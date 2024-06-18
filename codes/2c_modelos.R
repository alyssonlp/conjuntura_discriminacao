# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2024)
trimestre <- c(1)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    rds_file <- sprintf("pnadc%d_%d.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
# equacao salarial habitual - todos os trabalhos
    
    modelo_salarial <- glm(ln_r_hab_all ~ educ + educ_sq + educ_cubic + educ_forth +
                         potential_xp + married_woman + rural + metropolitan +
                         private_formal + private_informal + domestic_formal +
                         domestic_informal + public_service + self_employed  +
                         employer + agricultura + industria + construcao + 
                         comercio + servios_profissionais + transporte + 
                         servicos_pessoais_coletivos + adm_publica +
                         educ_saude + nonwhite + RO + AC + AM + RR + PA 
                         + AP + TO + MA + PI + CE + RN + PB + PE + AL + SE + 
                         BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO + DF, 
                       data = dt, weights = V1028)
 
    summary(modelo_salarial)   
    
# modelo empregabilidade - probabilidade em estar empregado
    
    modelo_empregabilidade <- glm(emp ~ educ + educ_sq + educ_cubic + educ_forth + 
                                potential_xp + married_woman + rural + metropolitan +
                                mother + RO + AC + AM + RR + PA + AP + TO + MA + 
                                PI + CE + RN + PB + PE + AL + SE + 
                                BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT +
                                GO + DF , data = dt, weights = V1028)
    
    summary(modelo_empregabilidade)
    
    rds_salarial <- sprintf("modelo_salarial_%d_%d.rds", aa, tri)
    saveRDS(modelo_salarial, file.path(intermediary_data, rds_salarial))
    
    rds_empregabilidade  <- sprintf("modelo_empregabilidade_%d_%d.rds", aa, tri)
    saveRDS(modelo_empregabilidade, file.path(intermediary_data, rds_empregabilidade))

    
# MASSA SALARIAL
# total de trabalhadores - com peso amostral
# vd4002 == 1 - pessoas ocupadas na semana de referência
workers <- dt[VD4002 == 1, sum(V1028)]
print(workers)
# rendimento medio habitual real - todos os trabalhos
y_bar <- med_conj_fun(dt, "r_hab_all", "V1028")
print(y_bar)


# calculando a massa salarial - individuos entre 25 e 54 anos
mass_salarial <- workers * y_bar
mass_salarial

# subsets - individuos negros
negros <- dt[nonwhite == 1, ]

# salario esperado para negros
var_x <- c("educ", "educ_sq" , "educ_cubic" , "educ_forth" ,
             "potential_xp" , "married_woman" , "rural" , "metropolitan" ,
             "private_formal" , "private_informal" , "domestic_formal" ,
             "domestic_informal" , "public_service" , "self_employed"  ,
             "employer" , "agricultura" , "industria" , "construcao" ,
             "comercio" , "servios_profissionais" , "transporte" , 
             "servicos_pessoais_coletivos" , "adm_publica" ,  "educ_saude" , 
             "RO" , "AC" , "AM" , "RR" , "PA"  , "AP" , 'TO' , "MA" , "PI" , 'CE' ,
             "RN" , "PB" , "PE" , "AL" , "SE" ,  "BA" , "MG" , "ES" , "RJ" , "SP" , 
             "PR" , "SC" , "RS" , "MS" , "MT" , "GO" , "DF")

var_x_negros_list <- list()

# calculando a media

for (var in var_x) {
  var_x_negros_list[[var]] <- med_var_fun(negros, var, "V1028" )
  x_negros[[var]] <- c(var_x_negros_list[[var]])
  file_name <- sprintf("resultado_%s_%d_%d_negros.csv", var, aa, tri)
  write.csv( var_x_negros_list[[var]], file.path(csv_files, file_name), row.names = FALSE)
}

# modelo salarial - negros
modelo_salarial_negros <- glm(ln_r_hab_all ~ educ + educ_sq + educ_cubic + educ_forth +
                         potential_xp + married_woman + rural + metropolitan +
                         private_formal + private_informal + domestic_formal +
                         domestic_informal + public_service + self_employed  +
                         employer + agricultura + industria + construcao + 
                         comercio + servios_profissionais + transporte + 
                         servicos_pessoais_coletivos + adm_publica +
                         educ_saude  + RO + AC + AM + RR + PA 
                       + AP + TO + MA + PI + CE + RN + PB + PE + AL + SE + 
                         BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO + DF, 
                       data = negros, weights = V1028)

summary(modelo_salarial_negros)

# coeficientes do modelo
coef_negros <- coef(modelo_salarial_negros)
num_var <- length(coef_negros) 
coef_x_negros<- numeric(num_var)

for (i in 0:num_var) {
  coef_x_negros[i] <- coef_negros[i+1]  
}

# Salario esperado para negros: E[yi | R = 1] = a + E[X|R=1]b + g
# rever esse somatorio
for (var in var_x_negros) {
E_sal_negros <- coef_x_negros[0] + sum(x_negros[var]*coef_x_negros[-1]) 
}


# SUBSET - BRANCOS
brancos <- dt[white == 1,]


var_x_brancos_list <- list()

# calculando a media

for (var in var_x) {
    var_x_brancos_list[[var]] <- med_var_fun(brancos, var, "V1028" )
    file_name <- sprintf("resultado_%s_%d_%d_brancos.csv", var, aa, tri)
    write.csv(var_x_brancos_list[[var]], file.path(csv_files, file_name), row.names = FALSE)
}


# modelo salarial - brancos
modelo_salarial_brancos <- glm(ln_r_hab_all ~ educ + educ_sq + educ_cubic + educ_forth +
                                potential_xp + married_woman + rural + metropolitan +
                                private_formal + private_informal + domestic_formal +
                                domestic_informal + public_service + self_employed  +
                                employer + agricultura + industria + construcao + 
                                comercio + servios_profissionais + transporte + 
                                servicos_pessoais_coletivos + adm_publica +
                                educ_saude  + RO + AC + AM + RR + PA 
                              + AP + TO + MA + PI + CE + RN + PB + PE + AL + SE + 
                                BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO + DF, 
                              data = brancos, weights = V1028)

summary(modelo_salarial_brancos)

# coeficientes do modelo
coef_brancos <- coef(modelo_salarial_brancos)
num_var <- length(coef_brancos) 
coef_x_brancos<- numeric(num_var)

for (i in 0:num_var) {
  coef_x_brancos[i] <- coef_brancos[i+1]  
}

# criar um looping que automatize todos os resultados das regressoes geradas
rds_reg_negros <- sprintf("modelo_salarial_negros_%d_%d.rds", aa, tri)
saveRDS(modelo_salarial, file.path(intermediary_data, rds_reg_negros))

rds_reg_brancos  <- sprintf("modelo_empregabilidade_brancos_%d_%d.rds", aa, tri)
saveRDS(modelo_empregabilidade, file.path(intermediary_data, rds_reg_brancos))


  }
}

