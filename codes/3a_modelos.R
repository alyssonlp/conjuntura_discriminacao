# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2024)
trimestre <- c(1)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=2){
      next  
    }
    
    rds_file <- sprintf("pnadc%d_%d.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
    # calculando a media do conjunto var_x
    var_x <- c("educ", "educ_sq" , "educ_cubic" , "educ_forth" ,
               "potential_xp" , "married_woman" , "rural" , "metropolitan" ,
               "private_formal" , "private_informal" , "domestic_formal" ,
               "domestic_informal" , "public_service" , "self_employed"  ,
               "employer" , "agricultura" , "industria" , "construcao" ,
               "comercio" , "servios_profissionais" , "transporte" , 
               "servicos_pessoais_coletivos" , "adm_publica" ,  "educ_saude" , 
               "RO" , "AC" , "AM" , "RR" , "PA"  , "AP" , 'TO' , "MA" , "PI" , 'CE' ,
               "RN" , "PB" , "PE" , "AL" , "SE" ,  "BA" , "MG" , "ES" , "RJ" , "SP" , 
               "PR" , "SC" , "RS" , "MS" , "MT" , "GO")
    
    raca_media_x <- dt[, lapply(.SD, function(x) wtd.mean(x, weights = V1028, na.rm = TRUE)), 
                       by = .(nonwhite == 1), .SDcols = var_x]
    raca_media_x[, nonwhite := as.numeric(nonwhite)]
    
    # calculando a media do conjunto var_y
    var_y <- c("educ" , "educ_sq" , "educ_cubic" , "educ_forth" , 
               "potential_xp" , "married_woman" , "rural" , "metropolitan" ,
               "mother" , "RO" , "AC" , "AM" , "RR" , "PA" , "AP" , "TO" , "MA" , 
               "PI" , "CE" , "RN" , "PB" , "PE" , "AL" , "SE" , "BA" , "MG" ,
               "ES" , "RJ" , "SP" , "PR" , "SC" , "RS" , "MS" , "MT" ,"GO")
    
    raca_media_y <- dt[, lapply(.SD, function(x) wtd.mean(x, weights = V1028, na.rm = TRUE)), 
                       by = .(nonwhite == 1), .SDcols = var_y]
    raca_media_y[, nonwhite := as.numeric(nonwhite)]
    
# equacao salarial habitual - todos os trabalhos
    # a funcao split permite separar as regressoes, por exemplo, por raca.
    # Caso queira tanto por raca, quanto ela inteira, usar fsplit
    
    modelo_salarial <- feols(ln_r_hab_all ~ nonwhite + .[var_x], 
                       data = dt, weights = ~V1028, fsplit = ~nonwhite)
    
    # exportando em latex, nao funciona com file.path, os tex sao salvos no diretorio
    salarial <- sprintf("modelo_salarial_%d_%d.tex", aa, tri)
    etable(modelo_salarial, file = salarial)
    
    coefs <- coef(modelo_salarial)
    coefs_bw <- select(coefs, -c(sample.var,nonwhite))
    
    
# modelo empregabilidade - probabilidade em estar empregado
    
    modelo_empregabilidade <- feols(emp ~ nonwhite + .[var_y]
                                    , data = dt, weights = ~V1028, , fsplit = ~nonwhite)
    
    empregabilidade <- sprintf("modelo_empregabilidade_%d_%d.tex", aa, tri)
    etable(modelo_empregabilidade, file = empregabilidade)

    coefs_emp <- coef(modelo_empregabilidade)
    coefs_emp_bw <- select(coefs_emp, -c(sample.var,nonwhite))
    
# Salario esperado para negros: E[yi|R=1] = a + E[X|R=1]b + gama

gama <-  coefs %>% 
  select(nonwhite) %>% 
  filter(!is.na(nonwhite)) 
gama <- as.matrix(gama)

raca_media_negros <- as.matrix(raca_media_x[1,])
matrix_negros <- as.matrix(coefs_bw[3,])
alfa_negros <- as.matrix(matrix_negros[,3])
matrix_negros_t <- t(matrix_negros)
# estava dando Error in raca_media_negros[, 2:51] %*% matrix_negros_t[4:53,] : 
#requer argumentos numéricos/complexos matriz/vetor
matrix_negros_t <- as.numeric(matrix_negros_t[4:53,])
alfa_negros <- as.numeric(alfa_negros)

e_wg_negros <- alfa_negros + (raca_media_negros[,2:51] %*% matrix_negros_t) + gama

# Salario esperado para brancos: E[yi|R=0] = a + E[X|R=0]b
raca_media_brancos <- as.matrix(raca_media_x[2,])
matrix_brancos <- as.matrix(coefs_bw[2,])
alfa_brancos <- as.numeric(matrix_brancos[,3])
matrix_brancos_t <- t(matrix_brancos)
matrix_brancos_t <- as.numeric(matrix_brancos_t[4:53,])

e_wg_brancos <- alfa_brancos + (raca_media_brancos[,2:51] %*% matrix_brancos_t)

# Probabilidade de trabalhar para negros

lower_delta <-  coefs_emp %>% 
  select(nonwhite) %>% 
  filter(!is.na(nonwhite)) 
lower_delta <- as.matrix(lower_delta)

raca_media_negros_y <- as.matrix(raca_media_y[1,])
matrix_negros_emp <- as.matrix(coefs_emp_bw[3,])
alfa_negros_emp <- as.numeric(matrix_negros_emp[,3])
matrix_negros_emp_t <- t(matrix_negros_emp)
matrix_negros_emp_t <- as.numeric(matrix_negros_emp_t[4:38,])

p_emp_negros <- alfa_negros_emp + 
  (raca_media_negros_y[,2:36] %*% matrix_negros_emp_t) + lower_delta


# Probabilidade de trabalhar para brancos

raca_media_brancos_y <- as.matrix(raca_media_y[2,])
matrix_brancos_emp <- as.matrix(coefs_emp_bw[2,])
alfa_brancos_emp <- as.numeric(matrix_brancos_emp[,3])
matrix_brancos_emp_t <- t(matrix_brancos_emp)
matrix_brancos_emp_t <- as.numeric(matrix_brancos_emp_t[4:38,])

p_emp_brancos <- alfa_brancos_emp + 
  (raca_media_brancos_y[,2:36] %*% matrix_brancos_emp_t) 

# Juntando os resultados do salario esperado e o da probabilidade em trabalhar
e_wg_brancos <- as.data.table(e_wg_brancos)
setnames(e_wg_brancos, "V1", "e_wg_white")
e_wg_negros <- as.data.table(e_wg_negros)
setnames(e_wg_negros, "nonwhite", "e_wg_nonwhite")
p_emp_brancos <- as.data.table(p_emp_brancos)
setnames(p_emp_brancos, "V1", "p_emp_white")
p_emp_negros <-as.data.table(p_emp_negros)
setnames(p_emp_negros, "nonwhite", "p_emp_nonwhite")

salario_esperado_empregabilidade <- cbind(e_wg_brancos, e_wg_negros[, .(e_wg_nonwhite)], 
                    p_emp_brancos[, .(p_emp_white)], p_emp_negros[, .(p_emp_nonwhite)])
salario_esperado_empregabilidade[, ano := aa]
salario_esperado_empregabilidade[, trimestre := tri]

salario_emp <- sprintf("resultados_salario_empregabilidade_%d_%d.csv", aa, tri)
write.csv(salario_esperado_empregabilidade, 
          file.path(csv_files, salario_emp), row.names = FALSE)

  }
}


