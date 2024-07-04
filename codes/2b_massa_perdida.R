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
    var_x <- c("nonwhite", "educ", "educ_sq",  "potential_xp" ,
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
    size_x <-length(var_x)
    
    # calculando a media do conjunto var_z
    var_z <- c("nonwhite" ,  "mother" ,"educ" , "educ_sq" , "potential_xp" ,
               "potential_xp_sq",  "married" , "rural" , "metropolitan" ,
               "RO" , "AC" , "AM" , "RR" , "PA" , "AP" , "TO" , "MA" , 
               "PI" , "CE" , "RN" , "PB" , "PE" , "AL" , "SE" , "BA" , "MG" ,
               "ES" , "RJ" , "SP" , "PR" , "SC" , "RS" , "MS" , "MT" ,"GO")
    size_z <- length(var_z)
    
    # Modelo Salarial
    modelo_salarial <- feols(ln_r_hab_all  ~ .[var_x], 
                             data = dt, weights = ~V1028, split = ~ female)
    
    beta_homem <- modelo_salarial[[1]]$coefficients 
    intercepto_homem <- as.matrix(beta_homem[[1]])
    gama_homem <- as.matrix(beta_homem[[2]])
    matrix_beta_homem <-  as.matrix(beta_homem) 
    
    beta_mulher <- modelo_salarial[[2]]$coefficients
    intercepto_mulher <- as.matrix(beta_mulher[[1]])
    gama_mulher <- as.matrix(beta_mulher[[2]])
    matrix_beta_mulher <- as.matrix(beta_mulher)
    
    # Media das covariadas do modelo salarial
    gender_race_media_x <- dt[, lapply(.SD, function(x) wtd.mean(x, weights = V1028, na.rm = TRUE)), 
                              by = .(gender_race), .SDcols = var_x]
    
    filtro_hb <- gender_race_media_x %>%  filter(gender_race == "Homem Branco")
    filtro_hn <- gender_race_media_x %>%  filter(gender_race == "Homem Negro")
    filtro_mb <- gender_race_media_x %>%  filter(gender_race == "Mulher Branca")
    filtro_mn <- gender_race_media_x %>%  filter(gender_race == "Mulher Negra")
    
    matrix_x_hb <- as.matrix(filtro_hb[, 3:(size_x+1)])
    matrix_x_hn <-  as.matrix(filtro_hn[, 3:(size_x+1)])
    
    matrix_x_mb <-  as.matrix(filtro_mb[, 3:(size_x+1)])
    matrix_x_mn <-  as.matrix(filtro_mn[, 3:(size_x+1)])
    
    # Salario Esperado - Homens
    # homem branco: E[yi|R=0] = a + E[X|R=0]b
    e_wg_hb <- intercepto_homem + matrix_x_hb%*%matrix_beta_homem[3:(size_x+1),]
    
    # homem negro: E[yi|R =1] = E[yi|R=0] + delta_x_beta + gama
    delta_x_beta_homem <- (matrix_x_hn - matrix_x_hb)%*%matrix_beta_homem[3:(size_x+1),]
    e_wg_hn <- e_wg_hb + delta_x_beta_homem + gama_homem
    
    # Salario Esperado - Mulheres
    # mulher branca: E[yi|R=0] = a + E[X|R=0]b
    e_wg_mb <- intercepto_mulher + matrix_x_mb%*%matrix_beta_mulher[3:(size_x+1),]
    
    # mulher negra: E[yi|R =1] = E[yi|R=0] + delta_x_beta + gama
    delta_x_beta_mulher<- (matrix_x_mn - matrix_x_mb)%*%matrix_beta_mulher[3:(size_x+1),]
    e_wg_mn <- e_wg_mb + delta_x_beta_mulher + gama_mulher
  
    
    # Modelo Empregabilidade
    modelo_empregabilidade <- feols(emp  ~ .[var_z], 
                                    data = dt, weights = ~V1028, split = ~ female)
    
    phi_homem <- modelo_empregabilidade[[1]]$coefficients 
    intercepto_emp_homem <- as.matrix(phi_homem[[1]])
    lower_delta_homem <- as.matrix(phi_homem[[2]])
    matrix_phi_homem <-  as.matrix(phi_homem) 
    
    phi_mulher <- modelo_empregabilidade[[2]]$coefficients
    intercepto_emp_mulher <- as.matrix(phi_mulher[[1]])
    lower_delta_mulher <- as.matrix(phi_mulher[[2]])
    matrix_phi_mulher <- as.matrix(phi_mulher)
    
    # Media das covariadas do modelo empregabilidade
    gender_race_media_z <- dt[, lapply(.SD, function(x) wtd.mean(x, weights = V1028, na.rm = TRUE)), 
                              by = .(gender_race), .SDcols = var_z]
    
    filtro_hb_emp <- gender_race_media_z %>%  filter(gender_race == "Homem Branco")
    filtro_hn_emp <- gender_race_media_z %>%  filter(gender_race == "Homem Negro")
    filtro_mb_emp <- gender_race_media_z %>%  filter(gender_race == "Mulher Branca")
    filtro_mn_emp <- gender_race_media_z %>%  filter(gender_race == "Mulher Negra")
    
    
    matrix_z_hb <- as.matrix(filtro_hb_emp[, 4:(size_z+1)])
    matrix_z_hn <- as.matrix(filtro_hn_emp[, 4:(size_z+1)])
    
    matrix_z_mb <-  as.matrix(filtro_mb_emp[, 3:(size_z+1)])
    matrix_z_mn <-  as.matrix(filtro_mn_emp[, 3:(size_z+1)])
    
    # Probabilidade em estar empregado - Homens
    # homem branco: P[Z|R=0] = teta + E[Z|R=0]phi
    p_emp_hb <- intercepto_emp_homem + matrix_z_hb%*%matrix_phi_homem[3:(size_z),]
    
    # homem negro: P[Z|R =1] = P[Z|R=0] + delta_z_phi + lower_delta
    delta_z_phi_homem <- (matrix_z_hn - matrix_z_hb)%*%matrix_phi_homem[3:(size_z),]
    p_emp_hn <- p_emp_hb + delta_z_phi_homem + lower_delta_homem
    
    # Probabilidade em estar empregada  - Mulheres
    # mulher branca: P[Z|R=0] = teta + E[Z|R=0]phi
    p_emp_mb <- intercepto_emp_mulher + matrix_z_mb%*%matrix_phi_mulher[3:(size_z+1),]
    
    # mulher negra: P[Z|R =1] = P[Z|R=0] + delta_z_phi + lower_delta
    delta_z_phi_mulher<- (matrix_z_mn - matrix_z_mb)%*%matrix_phi_mulher[3:(size_z+1),]
    p_emp_mn <- p_emp_mb + delta_z_phi_mulher + lower_delta_mulher
    
    # Massa salarial perdida - homens negros em relacao aos homens brancos
    # e_wg_hb*(delta_z_phi_homem + lower_delta_homem)*M_hn + p_emp_hn*M_hn*(delta_x_beta_homem + gama_homem)
    
    # M_hn = homens negros na economia
    
    M_hn <- sum(dt$V1028[dt$gender_race == "Homem Negro"])
    
    massa_perdida_hn <- exp(e_wg_hb)*(delta_z_phi_homem + lower_delta_homem)*M_hn +
      p_emp_hn*M_hn*(delta_x_beta_homem + gama_homem)
    
    # Massa salarial perdida - mulheres negras em relacao as mulheres brancas
    # e_wg_mb*(delta_z_phi_mulher + lower_delta_mulher)*M_mn + p_emp_mn*M_mn*(delta_x_beta_mulher + gama_mulher)
    
    # M_mn = mulheres negras na economia
    
    M_mn <- sum(dt$V1028[dt$gender_race == "Mulher Negra"])
    
    massa_perdida_mn <- exp(e_wg_mb)*(delta_z_phi_mulher + lower_delta_mulher)*M_mn +
      p_emp_mn*M_mn*(delta_x_beta_mulher + gama_mulher)
  
    
    anotri <- sprintf("%dT%d", aa, tri)
    dt2 <- data.table(Ano_trimestre = anotri, 
                      salario_esperado_medio_homem_branco = e_wg_hb,
                      salario_esperado_medio_homem_negro = e_wg_hn,
                      salario_esperado_medio_mulher_branca = e_wg_mb,
                      salario_esperado_medio_mulher_negra = e_wg_mn,
                      probabilidade_emprego_medio_homem_branco = p_emp_hb,
                      probabilidade_emprego_medio_homem_negro = p_emp_hn,
                      probabilidade_emprego_medio_mulher_branca = p_emp_mb,
                      probabilidade_emprego_mulher_negra = p_emp_mn,
                      massa_salarial_perdida_homem_negro = massa_perdida_hn,
                      massa_salarial_perdida_mulher_negra = massa_perdida_mn)
    
    dt1 <- rbind(dt1, dt2, fill = TRUE)
    }
}

fwrite(dt1, file.path(csv_files, "massa_salarial_perdida_genero_raca.csv"))
