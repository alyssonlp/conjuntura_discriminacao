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
    
    #aa = 2024
    #tri = 1
    
    # Incluir avisos:
    print(paste0("Computing statistics for year ", aa, 
                 " and quarter ", tri))
    
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
# HOMENS 
# SALARIOS
    
# Equação Salarial - Homens
    
    eq_wg_h <- lm(ln_r_hab_all ~ nonwhite + educ + educ_sq + potential_xp + 
                    potential_xp_sq + rural + metropolitan + diretores_gerentes + 
                    ciencias_intelectuais + tec_nivel_medio + apoio_adm + 
                    servicos_comercio + agropecuaria_pesca + operarios_construcao + 
                    maquinas_montadores + ocup_elementares + ffaa + private_formal + 
                    private_informal + domestic_formal + domestic_informal + public_service + 
                    self_employed + employer + agricultura + industria + 
                    construcao + comercio + servios_profissionais + transporte + 
                    servicos_pessoais_coletivos +  adm_publica + educ_saude + RO + AC +
                    AM + RR + PA + AP + TO + MA + PI + CE + RN + PB + PE + AL + 
                    SE + BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO, weights = V1028,
                  data = dt[male == 1 & is.na(ln_r_hab_all) == F])
    
    
    position_hn <- which(dt[male == 1 & is.na(ln_r_hab_all) == F,nonwhite == 1])
    position_hb <-  which(dt[male == 1 & is.na(ln_r_hab_all) == F,nonwhite == 0])
    max_pos_hn <- max(position_hn)
    
# Media salarial - homens negros, contrafactual e homens brancos
    media_wg_negros <- mean(exp(eq_wg_h$fitted.values[position_hn] + 
                                  eq_wg_h$residual[position_hn]))
    
    media_wg_negros_sem_discr <- mean(exp(eq_wg_h$fitted.values[position_hn] + 
                                (-1)*eq_wg_h$coefficients[2] +
                                eq_wg_h$residual[position_hn]))
    
    media_wg_brancos <- mean(exp(eq_wg_h$fitted.values[position_hb] + 
                                   eq_wg_h$residual[position_hb]))
    
# Componente discriminatório (gama)
    gama_homem <-  media_wg_negros - media_wg_negros_sem_discr
    
# Diferença na composicao (Delta_X_Beta)
    delta_x_beta_h <- media_wg_negros_sem_discr - media_wg_brancos
    
# Penalidade salarial devido a composicao e a discriminacao
    penalidade_salarial_homem <- gama_homem + delta_x_beta_h
    
# Homens negros na economia (P1)
    P1_negro <- dt[gender_race == "Homem Negro", sum(V1028)]
    
# Homens negros empregados
    p1_negro <- dt[gender_race == "Homem Negro",wtd.mean(pea_emp, weights = V1028)]
    
# Massa Salarial  perdida dos Homens Negros
    massa_salarial_perdida_hn <- P1_negro*penalidade_salarial_homem*p1_negro
    
# Massa salarial perdida devido ao efeito composicao
    massa_perda_composicao_hn <- P1_negro*delta_x_beta_h*p1_negro
    
# Massa salarial perdida devido ao efeito discriminacao
    massa_perda_discr_hn <- P1_negro*gama_homem*p1_negro
    
# EMPREGABILIDADE 
# Equação Probabilidade de estar empregado - homens
    
    eq_emp_h <- lm(pea_emp ~ nonwhite + parents + educ + educ_sq + potential_xp + 
                     potential_xp_sq + married + rural + metropolitan + RO + 
                     AC + AM + RR + PA + AP + TO + MA + PI + CE + RN + PB + PE + AL + 
                     SE + BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO, 
                   weights = V1028, data = dt[male == 1,])
    
    pos_hn_emp <- which(dt[male == 1, nonwhite == 1])
    pos_hb_emp <- which(dt[male == 1, nonwhite == 0])
    
# Probalidade empregabilidade - homens negros, contrafactual e homens brancos
    
    p_negros_emp <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
                               eq_emp_h$residual[pos_hn_emp])
    
    p_negros_sem_discr_emp <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
             (-1)*eq_emp_h$coefficients[2] + eq_emp_h$residual[pos_hn_emp])
    
    p_brancos_emp <- mean(eq_emp_h$fitted.values[pos_hb_emp] + 
                                eq_emp_h$residual[pos_hb_emp])
    
# Componente Discriminatorio (lower_delta)
    lower_delta_hn <-  p_negros_emp - p_negros_sem_discr_emp
    
# Diferença na composicao (delta_z_phi_h)
    delta_z_phi_h <- p_negros_sem_discr_emp - p_brancos_emp
    
# Penalidade na empregabilidade devido a composicao e a discriminacao
    penalidade_emp_hn <- lower_delta_hn + delta_z_phi_h
    
# Perda salarial do homem negro dada a empregabilidade
    emp_perdida_hn <- P1_negro*penalidade_emp_hn*media_wg_brancos
    
# Perda salarial do homem negro dada a empregabilidade e composicao
    emp_perdida_hn_composicao <- delta_z_phi_h*P1_negro*media_wg_brancos
    
# Perda salarial do homem negro dada a empregabilidade e discriminacao
    emp_perdida_hn_discr <- lower_delta_hn*P1_negro*media_wg_brancos
    
# Tamnho de homens brancos na economia
    P0_branco <- dt[gender_race == "Homem Branco", sum(V1028)]
    
# Massa contrafactual
    M_c_homem <- (P0_branco + P1_negro)*p_brancos_emp*media_wg_brancos
    
# Massa Salarial Total Perdida dos homens negros levando em conta a empregabilidade
    massa_total_perdida_hn <- M_c_homem + P1_negro*penalidade_salarial_homem + 
      P1_negro*penalidade_emp_hn
    
    
# ----------

# MULHERES
# SALARIOS
    
    # Equação Salarial - mulheres
    
    eq_wg_m <- lm(ln_r_hab_all ~ nonwhite + educ + educ_sq + potential_xp + 
                    potential_xp_sq + rural + metropolitan + diretores_gerentes + 
                    ciencias_intelectuais + tec_nivel_medio + apoio_adm + 
                    servicos_comercio + agropecuaria_pesca + operarios_construcao + 
                    maquinas_montadores + ocup_elementares + ffaa + private_formal + 
                    private_informal + domestic_formal + domestic_informal + public_service + 
                    self_employed + employer + agricultura + industria + 
                    construcao + comercio + servios_profissionais + transporte + 
                    servicos_pessoais_coletivos +  adm_publica + educ_saude + RO + AC +
                    AM + RR + PA + AP + TO + MA + PI + CE + RN + PB + PE + AL + 
                    SE + BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO, weights = V1028,
                  data = dt[male == 0 & is.na(ln_r_hab_all) == F])
    
    
    position_mn <- which(dt[male == 0 & is.na(ln_r_hab_all) == F,nonwhite == 1])
    position_mb <-  which(dt[male == 0 & is.na(ln_r_hab_all) == F,nonwhite == 0])
    max_pos_mn <- max(position_mn)
    
    # Media salarial - mulheres negras, contrafactual e mulheres brancas
    media_wg_negras <- mean(exp(eq_wg_m$fitted.values[position_mn] + 
                                  eq_wg_m$residual[position_mn]))
    
    media_wg_negras_sem_discr <- mean(exp(eq_wg_m$fitted.values[position_mn] + 
                                            (-1)*eq_wg_m$coefficients[2] +
                                            eq_wg_m$residual[position_mn]))
    
    media_wg_brancas <- mean(exp(eq_wg_m$fitted.values[position_mb] + 
                                   eq_wg_m$residual[position_mb]))
    
    # Componente discriminatório (gama)
    gama_mulher <-  media_wg_negras - media_wg_negras_sem_discr
    
    # Diferença na composicao (Delta_X_Beta)
    delta_x_beta_m <- media_wg_negras_sem_discr - media_wg_brancas
    
    # Penalidade salarial devido a composicao e a discriminacao
    penalidade_salarial_mulher <- gama_mulher + delta_x_beta_m
    
    # mulheres negras na economia (P1)
    P1_negra <- dt[gender_race == "Mulher Negra", sum(V1028)]
    
    # mulheres negras empregados
    p1_negra <- dt[gender_race == "Mulher Negra", wtd.mean(pea_emp, weights = V1028)]
    
    # Massa Salarial perdida das mulheres negras
    massa_salarial_perdida_mn <- P1_negra*penalidade_salarial_mulher*p1_negra
    
    # Massa salarial perdida devido ao efeito composicao
    massa_perda_composicao_mn <- P1_negra*delta_x_beta_m*p1_negra
    
    # Massa salarial perdida devido ao efeito discriminacao
    massa_perda_discr_mn <- P1_negra*gama_mulher*p1_negra
    
    # EMPREGABILIDADE 
    # Equação Probabilidade de estar empregado - mulheres
    
    eq_emp_m <- lm(pea_emp ~ nonwhite + parents + educ + educ_sq + potential_xp + 
                     potential_xp_sq + married + rural + metropolitan + RO + 
                     AC + AM + RR + PA + AP + TO + MA + PI + CE + RN + PB + PE + AL + 
                     SE + BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO, 
                   weights = V1028, data = dt[male == 0,])
    
    pos_mn_emp <- which(dt[male == 0, nonwhite == 1])
    pos_mb_emp <- which(dt[male == 0, nonwhite == 0])
    
    # Probalidade empregabilidade - mulheres negras, contrafactual e mulheres brancas
    
    p_negras_emp <- mean(eq_emp_m$fitted.values[pos_mn_emp] + 
                           eq_emp_m$residual[pos_mn_emp])
    
    p_negras_sem_discr_emp <- mean(eq_emp_m$fitted.values[pos_mn_emp] + 
                                     (-1)*eq_emp_m$coefficients[2] + eq_emp_m$residual[pos_mn_emp])
    
    p_brancas_emp <- mean(eq_emp_m$fitted.values[pos_mb_emp] + 
                            eq_emp_m$residual[pos_mb_emp])
    
    # Componente Discriminatorio (lower_delta)
    lower_delta_mn <-  p_negras_emp - p_negras_sem_discr_emp
    
    # Diferença na composicao (delta_z_phi_m)
    delta_z_phi_m <- p_negras_sem_discr_emp - p_brancas_emp
    
    # Penalidade na empregabilidade devido a composicao e a discriminacao
    penalidade_emp_mn <- lower_delta_mn + delta_z_phi_m
    
    # Perda salarial do Mulher Negra dada a empregabilidade
    emp_perdida_mn <- P1_negra*penalidade_emp_mn*media_wg_brancas
    
    # Perda salarial do Mulher Negra dada a empregabilidade e composicao
    emp_perdida_mn_composicao <- delta_z_phi_m*P1_negra*media_wg_brancas
    
    # Perda salarial do Mulher Negra dada a empregabilidade e discriminacao
    emp_perdida_mn_discr <- lower_delta_mn*P1_negra*media_wg_brancas
    
    # Tamnho de mulheres brancas na economia
    P0_branca <- dt[gender_race == "Mulher Branca", sum(V1028)]
    
    # Massa contrafactual
    M_c_mulher <- (P0_branca + P1_negra)*p_brancas_emp*media_wg_brancas
    
    # Massa Salarial Total Perdida dos mulheres negras levando em conta a empregabilidade
    massa_total_perdida_mn <- M_c_mulher + P1_negra*penalidade_salarial_mulher + 
      P1_negra*penalidade_emp_mn
    
    anotri <- sprintf("%dT%d", aa, tri)
    bi <- 1000000000
    dt2 <- data.table(Ano_trimestre = anotri, 
                      massa_wg_perdida_hn = round(massa_salarial_perdida_hn/bi, 2),
                      massa_wg_composicao_hn = round(massa_perda_composicao_hn/bi, 2),
                      massa_wg_discriminacao_hn = round(massa_perda_discr_hn/bi, 2),
                      massa_emp_perdida_hn = round(emp_perdida_hn/bi, 2),
                      massa_emp_composicao_hn = round(emp_perdida_hn_composicao/bi, 2),
                      massa_emp_discriminacao_hn = round(emp_perdida_hn_discr/bi, 2),
                      total_perdido_hn = massa_wg_perdida_hn + massa_emp_perdida_hn,
                      massa_wg_perdida_mn = round(massa_salarial_perdida_mn/bi, 2),
                      massa_wg_composicao_mn = round(massa_perda_composicao_mn/bi, 2),
                      massa_wg_discriminacao_mn = round(massa_perda_discr_mn/bi, 2),
                      massa_emp_perdida_mn = round(emp_perdida_mn/bi, 2),
                      massa_emp_composicao_mn = round(emp_perdida_mn_composicao/bi, 2),
                      massa_emp_discriminacao_mn = round(emp_perdida_mn_discr/bi, 2),
                      total_perdido_mn = massa_wg_perdida_mn + massa_emp_perdida_mn,
                      composicao_hn_wg = round(delta_x_beta_h, 2),
                      discriminacao_hn_wg = round(gama_homem, 2) ,
                      penalidade_salarial_hn = round(penalidade_salarial_homem, 2),
                      composicao_hn_emp = round(100*delta_z_phi_h, 2),
                      discriminacao_hn_emp = round(100*lower_delta_hn, 2),
                      penalidade_emp_hn = round(100*penalidade_emp_hn, 2),
                      composicao_mn_wg = round(delta_x_beta_m, 2),
                      discriminacao_mn_wg = round(gama_mulher, 2) ,
                      penalidade_salarial_mn = round(penalidade_salarial_mulher, 2),
                      composicao_mn_emp = round(100*delta_z_phi_m, 2),
                      discriminacao_mn_emp = round(100*lower_delta_mn, 2),
                      penalidade_emp_mn = round(100*penalidade_emp_mn, 2))
                      
    dt1 <- rbind(dt1, dt2, fill = TRUE)
    
  }
}

fwrite(dt1, file.path(csv_output, "resultados_massa_salarial.csv"))
