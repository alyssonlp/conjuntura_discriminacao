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
    media_wg_hn <- mean(exp(eq_wg_h$fitted.values[position_hn] + 
                                  eq_wg_h$residual[position_hn]))
    
    media_wg_hn_sem_discr <- mean(exp(eq_wg_h$fitted.values[position_hn] + 
                                (-1)*eq_wg_h$coefficients[2] +
                                eq_wg_h$residual[position_hn]))
    
    media_wg_hb <- mean(exp(eq_wg_h$fitted.values[position_hb] + 
                                   eq_wg_h$residual[position_hb]))
    
# Componente discriminatório 
    discriminacao_wg_hn <-  media_wg_hn - media_wg_hn_sem_discr
    
# Diferença na composicao 
    composicao_wg_hn <- media_wg_hn_sem_discr - media_wg_hb
    
# Penalidade salarial devido a composicao e a discriminacao
    penalidade_wg_hn <- discriminacao_wg_hn + composicao_wg_hn
    
# Homens negros na economia (P1)
    P1_hn <- dt[gender_race == "Homem Negro", sum(V1028)]
    
# Homens negros empregados
    e1_hn <- dt[gender_race == "Homem Negro",wtd.mean(pea_emp, weights = V1028)]
    
# Massa Salarial  perdida dos Homens Negros
    wg_massa_perdida_hn <-  P1_hn*penalidade_wg_hn*e1_hn
    
# Massa salarial perdida devido ao efeito composicao
    composicao_massa_wg_hn <- P1_hn*composicao_wg_hn*e1_hn
    
# Massa salarial perdida devido ao efeito discriminacao
    discriminacao_massa_wg_hn <- P1_hn*discriminacao_wg_hn*e1_hn
    
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
    
    media_emp_hn <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
                               eq_emp_h$residual[pos_hn_emp])
    
    media_emp_hn_sem_discr <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
             (-1)*eq_emp_h$coefficients[2] + eq_emp_h$residual[pos_hn_emp])
    
    media_emp_hb <- mean(eq_emp_h$fitted.values[pos_hb_emp] + 
                                eq_emp_h$residual[pos_hb_emp])
    
# Componente Discriminatorio 
    discriminacao_emp_hn <- media_emp_hn - media_emp_hn_sem_discr
    
# Diferença na composicao 
    composicao_emp_hn <- media_emp_hn_sem_discr - media_emp_hb
    
# Penalidade na empregabilidade devido a composicao e a discriminacao
    penalidade_emp_hn <- discriminacao_emp_hn + composicao_emp_hn
    
# Perda salarial do homem negro dada a empregabilidade
    emp_massa_perdida_hn <- P1_hn*penalidade_emp_hn*media_wg_hb
    
# Perda salarial do homem negro dada a empregabilidade - efeito composicao
    composicao_massa_emp_hn <- P1_hn*composicao_emp_hn*media_wg_hb
    
# Perda salarial do homem negro dada a empregabilidade e discriminacao
    discriminacao_massa_emp_hn <- P1_hn*discriminacao_emp_hn*media_wg_hb

    
# Massa Salarial Total Perdida dos homens negros levando em conta a empregabilidade
    total_hn <-  discriminacao_massa_wg_hn + composicao_massa_wg_hn + 
      discriminacao_massa_emp_hn + composicao_massa_emp_hn
    
    
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
    media_wg_mn <- mean(exp(eq_wg_m$fitted.values[position_mn] + 
                                  eq_wg_m$residual[position_mn]))
    
    media_wg_mn_sem_discr <- mean(exp(eq_wg_m$fitted.values[position_mn] + 
                                            (-1)*eq_wg_m$coefficients[2] +
                                            eq_wg_m$residual[position_mn]))
    
    media_wg_mb <- mean(exp(eq_wg_m$fitted.values[position_mb] + 
                                   eq_wg_m$residual[position_mb]))
    
    # Componente discriminatório 
    discriminacao_wg_mn <-  media_wg_mn - media_wg_mn_sem_discr 
    
    # Diferença na composicao 
    composicao_wg_mn <- media_wg_mn_sem_discr - media_wg_mb
    
    # Penalidade salarial devido a composicao e a discriminacao
    penalidade_wg_mn <- discriminacao_wg_mn + composicao_wg_mn
    
    # mulheres negras na economia (P1)
    P1_mn <- dt[gender_race == "Mulher Negra", sum(V1028)]
    
    # mulheres negras empregados
    e1_mn <- dt[gender_race == "Mulher Negra", wtd.mean(pea_emp, weights = V1028)]
    
    # Massa Salarial perdida das mulheres negras
    wg_massa_perdida_mn <- P1_mn*penalidade_wg_mn*e1_mn
    
    # Massa salarial perdida devido ao efeito composicao
    composicao_massa_wg_mn <- P1_mn*composicao_wg_mn*e1_mn
    
    # Massa salarial perdida devido ao efeito discriminacao
    discriminacao_massa_wg_mn <- P1_mn*discriminacao_wg_mn*e1_mn
    
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
    
    media_emp_mn <- mean(eq_emp_m$fitted.values[pos_mn_emp] + 
                           eq_emp_m$residual[pos_mn_emp])
    
    media_emp_mn_sem_discr <- mean(eq_emp_m$fitted.values[pos_mn_emp] + 
                                     (-1)*eq_emp_m$coefficients[2] + eq_emp_m$residual[pos_mn_emp])
    
    media_emp_mb <- mean(eq_emp_m$fitted.values[pos_mb_emp] + 
                            eq_emp_m$residual[pos_mb_emp])
    
    # Componente Discriminatorio 
    discriminacao_emp_mn <-  media_emp_mn - media_emp_mn_sem_discr
    
    # Diferença na composicao 
    composicao_emp_mn <- media_emp_mn_sem_discr - media_emp_mb
    
    # Penalidade na empregabilidade devido a composicao e a discriminacao
    penalidade_emp_mn <- discriminacao_emp_mn + composicao_emp_mn
    
    # Perda salarial da Mulher Negra dada a empregabilidade
    emp_massa_perdida_mn <- P1_mn*penalidade_emp_mn*media_wg_mb
    
    # Perda salarial da Mulher Negra dada a empregabilidade - efeito composicao
    composicao_massa_emp_mn <- P1_mn*composicao_emp_mn*media_wg_mb
    
    # Perda salarial do Mulher Negra dada a empregabilidade - efeito discriminacao
    discriminacao_massa_emp_mn <- P1_mn*discriminacao_emp_mn*media_wg_mb
    
    
    # Massa Salarial Total Perdida dos mulheres negras 
    total_mn <-  discriminacao_massa_wg_mn + composicao_massa_wg_mn + 
      discriminacao_massa_emp_mn + composicao_massa_emp_mn
    
    anotri <- sprintf("%dT%d", aa, tri)
    bi <- 1000000000
    dt2 <- data.table(Ano_trimestre = anotri, 
                      total_hn = round(total_hn/bi, 2),
                      discriminacao_massa_wg_hn = round(discriminacao_massa_wg_hn/bi, 2),
                      composicao_massa_wg_hn = round(composicao_massa_wg_hn/bi, 2),
                      discriminacao_massa_emp_hn = round(discriminacao_massa_emp_hn/bi, 2),
                      composicao_massa_emp_hn = round(composicao_massa_emp_hn/bi, 2),
                      total_mn = round(total_mn/bi, 2),
                      discriminacao_massa_wg_mn = round(discriminacao_massa_wg_mn/bi, 2),
                      composicao_massa_wg_mn = round(composicao_massa_wg_mn/bi, 2),
                      discriminacao_massa_emp_mn = round(discriminacao_massa_emp_mn/bi, 2),
                      composicao_massa_emp_mn = round(composicao_massa_emp_mn/bi, 2),
                      composicao_wg_hn = round(composicao_wg_hn, 2),
                      discriminacao_wg_hn = round(discriminacao_wg_hn, 2) ,
                      composicao_emp_hn = round(composicao_emp_hn, 2),
                      discriminacao_emp_hn = round(discriminacao_emp_hn, 2),
                      penalidade_emp_hn = round(penalidade_emp_hn, 2),
                      composicao_wg_mn = round(composicao_wg_mn, 2),
                      discriminacao_wg_mn = round(discriminacao_wg_mn, 2) ,
                      composicao_emp_mn = round(composicao_emp_mn, 2),
                      discriminacao_emp_mn = round(discriminacao_emp_mn, 2))
                      
    dt1 <- rbind(dt1, dt2, fill = TRUE)
    
  }
}

fwrite(dt1, file.path(csv_output, "resultados_massa_salarial.csv"))
