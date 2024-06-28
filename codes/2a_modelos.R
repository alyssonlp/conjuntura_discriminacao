dt1 <- data.table()

# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2012:2024)
trimestre <- c(1:4)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=2){
      next  
    }
    
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(original_data, rds_file)))

# calculando a media do conjunto var_x
var_x <- c("educ", "educ_sq", "nonwhite",  "potential_xp" , "potential_xp_sq",
           "rural" , "metropolitan" , "private_formal" , "private_informal" , 
           "domestic_formal" , "domestic_informal" , "public_service" , "self_employed"  ,
           "employer" , "agricultura" , "industria" , "construcao" ,
           "comercio" , "servios_profissionais" , "transporte" , 
           "servicos_pessoais_coletivos" , "adm_publica" ,  "educ_saude" , 
           "RO" , "AC" , "AM" , "RR" , "PA"  , "AP" , 'TO' , "MA" , "PI" , 'CE' ,
           "RN" , "PB" , "PE" , "AL" , "SE" ,  "BA" , "MG" , "ES" , "RJ" , "SP" , 
           "PR" , "SC" , "RS" , "MS" , "MT" , "GO")

# calculando a media do conjunto var_y
var_y <- c("educ" , "educ_sq" , "nonwhite" , "potential_xp" , "potential_xp_sq",
           "married" , "rural" , "metropolitan" ,
           "mother" , "RO" , "AC" , "AM" , "RR" , "PA" , "AP" , "TO" , "MA" , 
           "PI" , "CE" , "RN" , "PB" , "PE" , "AL" , "SE" , "BA" , "MG" ,
           "ES" , "RJ" , "SP" , "PR" , "SC" , "RS" , "MS" , "MT" ,"GO")

#media_x <- dt[, lapply(.SD, function(x) wtd.mean(x, weights = V1028, na.rm = TRUE)), 
                  # by = .(male, nonwhite), .SDcols = var_x]


# Renda
modelo_salarial <- feols(ln_r_hab_all ~  .[var_x], 
                         data = dt, weights = ~V1028, split = ~male)


intercept_male <- modelo_salarial[[2]]$coefficients[1]
intercept_female <- modelo_salarial[[1]]$coefficients[1]
nonwhite_female <-modelo_salarial[[1]]$coefficients[4]

# hiato  de renda mulher branca
female_male_white <- intercept_female - intercept_male

# hiato de renda mulher negra
female_black_male_white <- nonwhite_female + female_male_white

# hiato de renda homem negro
nonwhite_male <-modelo_salarial[[2]]$coefficients[4]


# Empregabilidade
modelo_empregabilidade <- feols(emp ~  .[var_y], 
                                data = dt, weights = ~V1028, split = ~male)

intercept_male_emp <- modelo_empregabilidade[[2]]$coefficients[1]
intercept_female_emp <- modelo_empregabilidade[[1]]$coefficients[1]
nonwhite_female_emp <-modelo_empregabilidade[[1]]$coefficients[4]

# hiato de empregabilidade mulher branca
female_male_white_emp <- intercept_female_emp - intercept_male_emp

# hiato de empregabilidade mulher negra
female_black_male_white_emp <- nonwhite_female_emp + female_male_white_emp

# hiato de empregabilidade homem negro
nonwhite_male_emp <-modelo_empregabilidade[[2]]$coefficients[4]

Ano_trimestre  <-c("Ano_trimestre")  
hiato_renda_mb <- c("hiato_renda_mb") 
hiato_renda_mn <- c("hiato_renda_mn")
hiato_renda_hn <- c("hiato_renda_hn")
hiato_empregabilidade_mb <- c("hiato_empregabilidade_mb")
hiato_empregabilidade_mn <- c("hiato_empregabilidade_mn")
hiato_empregabilidade_hn <- c("hiato_empregabilidade_hn")

anotri <- sprintf("%dT%d", aa, tri)
dt2 <- data.table(Ano_trimestre = anotri, hiato_renda_mb = female_male_white,
                     hiato_renda_mn = female_black_male_white, hiato_renda_hn = nonwhite_male,
                     hiato_empregabilidade_mb = female_male_white_emp, 
                     hiato_empregabilidade_mn = female_black_male_white_emp, 
                     hiato_empregabilidade_hn = nonwhite_male_emp)

dt1 <- rbind(dt1, dt2, fill = TRUE)
  }
}

fwrite(dt1, file.path(csv_files, "hiatos.csv"))
