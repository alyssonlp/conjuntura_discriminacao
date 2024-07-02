dt1 <- data.table()

# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2023)
trimestre <- c(1)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=2){
      next  
    }
    
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(original_data, rds_file)))
  
    # calculando a media do conjunto var_x - adicionar ocupacoes
    var_x <- c("educ", "educ_sq", "educ_cubic", "educ_fourth", "nonwhite",  "potential_xp" ,
               "potential_xp_sq","rural" , "metropolitan" ,"diretores_gerentes", "ciencias_intelectuais", 
               "tec_nivel_medio", "apoio_adm", "servicos_comercio", "agropecuaria_pesca", 
               "operarios_construcao", "maquinas_montadores", "ocup_elementares", "ffaa",
               "private_formal" , "private_informal" , 
               "domestic_formal" , "domestic_informal" , "public_service" , "self_employed"  ,
               "employer" , "agricultura" , "industria" , "construcao" ,
               "comercio" , "servios_profissionais" , "transporte" , 
               "servicos_pessoais_coletivos" , "adm_publica" ,  "educ_saude" , 
               "RO" , "AC" , "AM" , "RR" , "PA"  , "AP" , 'TO' , "MA" , "PI" , 'CE' ,
               "RN" , "PB" , "PE" , "AL" , "SE" ,  "BA" , "MG" , "ES" , "RJ" , "SP" , 
               "PR" , "SC" , "RS" , "MS" , "MT" , "GO")
    
    # calculando a media do conjunto var_z
    var_z <- c("educ" , "educ_sq" , "educ_cubic", "educ_fourth", "nonwhite" , "potential_xp" ,
               "potential_xp_sq",  "married" , "rural" , "metropolitan" ,
               "mother" , "RO" , "AC" , "AM" , "RR" , "PA" , "AP" , "TO" , "MA" , 
               "PI" , "CE" , "RN" , "PB" , "PE" , "AL" , "SE" , "BA" , "MG" ,
               "ES" , "RJ" , "SP" , "PR" , "SC" , "RS" , "MS" , "MT" ,"GO")
    
    # Renda - homens e mulheres
    modelo_salarial <- feols(ln_r_hab_all ~ female +.[var_x], 
                             data = dt, weights = ~V1028, fsplit = ~ female)
    coefs <- coef(modelo_salarial)
    
    gender_media_x <- dt[, lapply(.SD, function(x) wtd.mean(x, weights = V1028, na.rm = TRUE)), 
                       by = .(female), .SDcols = var_x]
    gender_media_x[, female := as.numeric(female)]
    
    
    # salario esperado
    size_x <-length(var_x)
    matrix_betas <- as.matrix(coefs)
    betas <- as.matrix(as.numeric(matrix_betas[1,6:(size_x+5)]))
    
    # Salario esperado para mulheres : E[yi|R=1] = a + E[X|R=1]b + gama
    matrix_x_mulher <- as.matrix(gender_media_x[2,2:(size_x+1)])
    
    alfa_mulher <- as.matrix(as.numeric(matrix_betas[3,4]))
    gama <- as.matrix(as.numeric(matrix_betas[1,5]))

    
    e_wg_mulher <- alfa_mulher + (matrix_x_mulher%*%betas) - gama
    
    # Salario esperado para homens: E[yi|R=0] = a + E[X|R=0]b
    matrix_x_homem <- as.matrix(gender_media_x[1, 2:(size_x+1)])
    alfa_homem <- as.matrix(as.numeric(matrix_betas[2,4]))
    
    e_wg_homem <- alfa_homem + (matrix_x_homem%*%betas)
    
    # Delta_x*beta = (E[X|R=1] - E[X|R=0])*beta
    Delta_X_beta <-  as.numeric((matrix_x_mulher - matrix_x_homem)%*%betas)
    
    
    # Empregabilidade
    modelo_empregabilidade <- feols(emp ~ female +  .[var_z], 
                                    data = dt, weights = ~V1028, fsplit = ~ female)
    coefs_emp <- coef(modelo_empregabilidade)
    
    gender_media_z <- dt[, lapply(.SD, function(x) wtd.mean(x, weights = V1028, na.rm = TRUE)), 
                         by = .(female), .SDcols = var_z]
    gender_media_z[, female := as.numeric(female)]
    
    
    # Probabilidade em trabalhar
    size_z <-length(var_z)
    matrix_phi <- as.matrix(coefs_emp)
    phi <- as.matrix(as.numeric(matrix_phi[1,6:(size_z+5)]))

    # Probabilidade esperada de trabalhar para mulheres: P[Z|R= 1] = teta + E[Z|R=1]phi + lower_delta
    matrix_z_mulher <- as.matrix(gender_media_z[2,2:(size_z+1)])
    
    teta_mulher <- as.matrix(as.numeric(matrix_phi[3,4]))
    lower_delta <- as.matrix(as.numeric(matrix_phi[1,5]))
    
    p_emp_mulher <- teta_mulher + (matrix_z_mulher%*%phi) + lower_delta
    
    # Probabilidade esperada de trabalhar para homens: P[Z|R=0] = teta + E[Z|R=0]phi
    matrix_z_homem <- as.matrix(gender_media_z[1, 2:(size_z+1)])
    teta_homem <- as.matrix(as.numeric(matrix_phi[2,4]))
    
    p_emp_homem <- teta_homem + (matrix_z_homem%*%phi)
    
    # Delta_z*phi = (P[Z|R=1] - P[Z|R=0])*phi
    Delta_Z_phi <-  as.numeric((matrix_z_mulher - matrix_z_homem)%*%phi)
    
    # Massa salarial perdida
    # e_wg_mulher*(Delta_Z_phi + lower_delta)*M_1 + p_emp_homem*M_1*(Delta_X_beta + gama)
    
    # M_1 = mulheres na economia
    
    M_1 <- sum(dt$V1028[dt$female == 1])
    
    massa_perdida_mulher <- (exp(e_wg_mulher))*(Delta_Z_phi + lower_delta)*M_1 + p_emp_homem*M_1*(Delta_X_beta + gama)
    salario_perdido_mulher <- (exp(e_wg_mulher))*(Delta_Z_phi + lower_delta)*M_1 
    massa_perdida_emp_equivalente <- mass_perdida_mulher - salario_perdido_mulher
  
    massa_perdida <- c("massa_perdida_mulher")
    salario_perdido_devido_menor_p_emp <- C("salario_perdido_mulher")
    massa_perdida_emp_equivalente_devido_discr <- c("massa_perdida_emp_equivalente")
    anotri <- sprintf("%dT%d", aa, tri)
    dt2 <- data.table(Ano_trimestre = anotri, massa_perdida = massa_perdida_mulher,
                      salario_perdido_devido_menor_p_emp = salario_perdido_mulher,
                      massa_perdida_emp_equivalente_devido_discr = massa_perdida_emp_equivalente)
    
    dt1 <- rbind(dt1, dt2, fill = TRUE)
    }
}

fwrite(dt1, file.path(csv_files, "massa_salarial_perdida_genero.csv"))
