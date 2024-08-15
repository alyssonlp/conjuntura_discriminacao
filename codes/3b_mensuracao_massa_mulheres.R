dt1 <- data.table()

# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2012:2024)
trimestre <- c(1:4)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    
    if(aa == 2024 & tri >=3){
      next  
    }
    
    #aa = 2024
    #tri = 1
    
    # Incluir avisos:
    print(paste0("Computing statistics for year ", aa, 
                 " and quarter ", tri))
    
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    dt <- setDT(dt)[V2009 >= 25 & V2009 <= 65]
    
    # mulheres - SALARIOS
    
    eq_wg_m <- lm(ln_r_hab_all ~ nonwhite + educ + educ_sq + potential_xp + 
                    potential_xp_sq + rural + metropolitan + diretores_gerentes + 
                    ciencias_intelectuais + tec_nivel_medio + apoio_adm + 
                    servicos_comercio + agropecuaria_pesca + operarios_construcao + 
                    maquinas_montadores + ocup_elementares + ffaa + private_formal + 
                    private_informal + domestic_formal + domestic_informal + public_service + 
                    self_employed + employer + agricultura + industria + 
                    construcao + comercio + servios_profissionais + transporte + 
                    servicos_pessoais_coletivos +  adm_publica + educ_saude +
                    factor(UF),
                  weights = V1028,
                  data = dt[male == 0 & is.na(ln_r_hab_all) == F])
    
    
    pos_mn <- which(as.data.table(eq_wg_m[["model"]])[, nonwhite] == 1)
    pos_mb <- which(as.data.table(eq_wg_m[["model"]])[, nonwhite] == 0)
    pos_m <-  which(as.data.table(eq_wg_m[["model"]])[, nonwhite] == 0 |
                      as.data.table(eq_wg_m[["model"]])[, nonwhite] == 1)
    
    pos_dt_mn <- which(dt[, male == 0 & is.na(r_hab_all) == F & nonwhite == 1])
    pos_dt_mb <- which(dt[, male == 0 & is.na(r_hab_all) == F & nonwhite == 0])
    pos_dt_m <- which(dt[, male == 0 & is.na(r_hab_all) == F])
    
    #### mulheres brancas SALARIOS - Contrafactual: mulheres ----
    
    media_wg_mb <- mean(exp(eq_wg_m$fitted.values[pos_mb] + 
                              eq_wg_m$residual[pos_mb]),
                        weights = dt[pos_dt_mb, V1028])
    
    media_wg_mulheres <- mean(exp(eq_wg_m$fitted.values[pos_m] + 
                                  eq_wg_m$residual[pos_m]),
                              weights = dt[pos_dt_m, V1028])
    
    # Obs. mulheres brancas nao tem efeito discriminacao
    
    # mulheres brancas na economia (P_0)
    P0_mb <- dt[male == 0 & nonwhite == 0, sum(V1028)]
    
    # mulheres brancas empregados (e_0)
    e0_mb <- dt[male == 0 & nonwhite == 0,wtd.mean(pea_emp, weights = V1028)]
    
    # Diferença na composicao
    composicao_wg_mb <- media_wg_mb - media_wg_mulheres
    
    # Massa salarial perdida devido ao efeito composicao
    composicao_massa_wg_mb <- P0_mb*composicao_wg_mb*e0_mb
    
    # Massa salarial total perdida(premiada) mulheres brancas
    wg_massa_perdida_mb <- composicao_massa_wg_mb + 0
    
    #### mulheres negras SALARIOS - Contrafactual: mulheres ----
    # mulheres negras na economia (P_1)
    P1_mn <- dt[male == 0 & nonwhite == 1, sum(V1028)]
    
    # mulheres negras empregados (e_1)
    e1_mn <- dt[male == 0 & nonwhite == 1, wtd.mean(pea_emp, weights = V1028)]
    
    
    media_wg_mn <- mean(exp(eq_wg_m$fitted.values[pos_mn] + 
                              eq_wg_m$residual[pos_mn]),
                        weights = dt[pos_dt_mn, V1028])
    
    media_wg_mn_sem_discr <- mean(exp(eq_wg_m$fitted.values[pos_mn] + 
                                        (-1)*eq_wg_m$coefficients[2] +
                                        eq_wg_m$residual[pos_mn]),
                                  weights = dt[pos_dt_mn, V1028])
    
    # Componente discriminatório 
    discriminacao_wg_mn <-  media_wg_mn - media_wg_mn_sem_discr 
    
    # Diferença na composicao
    composicao_wg_mn <- media_wg_mn_sem_discr - media_wg_mulheres
    
    # Penalidade salarial devido a composicao e a discriminacao
    penalidade_wg_mn <- discriminacao_wg_mn + composicao_wg_mn
    
    # Massa Salarial  perdida dos mulheres negras
    wg_massa_perdida_mn <- P1_mn*penalidade_wg_mn*e1_mn
    
    # Massa salarial perdida devido ao efeito composicao
    composicao_massa_wg_mn <- P1_mn*composicao_wg_mn*e1_mn
    
    # Massa salarial perdida devido ao efeito discriminacao
    discriminacao_massa_wg_mn <- P1_mn*discriminacao_wg_mn*e1_mn
    
    #### EMPREGABILIDADE -----
    # Equação Probabilidade de estar empregado - mulheres
    
    eq_emp_m <- lm(pea_emp ~ nonwhite + parents + educ + educ_sq + potential_xp + 
                     potential_xp_sq + married + rural + metropolitan +
                     estudando + factor(UF), 
                   weights = V1028, data = dt[male == 0,])
    
    pos_mn_emp <- which(as.data.table(eq_emp_m[["model"]])[, nonwhite] == 1)
    pos_mb_emp <- which(as.data.table(eq_emp_m[["model"]])[, nonwhite] == 0)
    pos_m_emp  <- which(as.data.table(eq_emp_m[["model"]])[, nonwhite] == 0 |
                          as.data.table(eq_emp_m[["model"]])[, nonwhite] == 1)
    
    pos_dt_mn_emp <- which(dt[, male == 0 & nonwhite == 1])
    pos_dt_mb_emp <- which(dt[, male == 0 & nonwhite == 0])
    pos_dt_m_emp  <- which(dt[, male == 0])

    
    #### mulheres Brancos EMPREGO - Contrafactual: mulheres ----
    
    media_emp_mb <- mean(eq_emp_m$fitted.values[pos_mb_emp] + 
                           eq_emp_m$residual[pos_mb_emp],
                         weights = dt[pos_dt_mb_emp, V1028])
    
    media_emp_mulheres <- mean(eq_emp_m$fitted.values[pos_m_emp] + 
                               eq_emp_m$residual[pos_m_emp],
                            weights = dt[pos_dt_m_emp, V1028])
    
    # Efeito discriminacao == 0
    # Diferença na composicao
    composicao_emp_mb <- media_emp_mb - media_emp_mulheres
    
    # Perda salarial do homem branco dada a empregabilidade
    emp_massa_perdida_mb <- P0_mb*composicao_emp_mb*media_wg_mulheres
    
    # Perda salarial do homem branco dada a empregabilidade - efeito composicao
    composicao_massa_emp_mb <- emp_massa_perdida_mb + 0
    
    # Massa Salarial Total Perdida (ou premiada) dos mulheres brancos
    total_mb <- composicao_massa_wg_mb + composicao_massa_emp_mb
    
    #### mulheres negras  EMPREGO - Contrafactual: mulheres ----
    
    media_emp_mn <- mean(eq_emp_m$fitted.values[pos_mn_emp] + 
                           eq_emp_m$residual[pos_mn_emp],
                         weights = dt[pos_dt_mn_emp, V1028])
    
    media_emp_mn_sem_discr <- mean(eq_emp_m$fitted.values[pos_mn_emp] + 
                                     (-1)*eq_emp_m$coefficients[2] +
                                     eq_emp_m$residual[pos_mn_emp],
                                   weights = dt[pos_dt_mn_emp, V1028])
  
    
    # Componente discriminatorio
    discriminacao_emp_mn <- media_emp_mn - media_emp_mn_sem_discr
    
    # Diferença na composicao
    composicao_emp_mn <- media_emp_mn_sem_discr - media_emp_mulheres
    
    # penalidade na empregaabilidade - homem negro
    penalidade_emp_mn <- discriminacao_emp_mn + composicao_emp_mn
    
    # Perda salarial do homem negro dada a empregabilidade
    emp_massa_perdida_mn <- P1_mn*penalidade_emp_mn*media_wg_mulheres
    
    # Perda salarial do homem negro dada a empregabilidade - efeito discriminacao
    discriminacao_massa_emp_mn <- P1_mn*discriminacao_emp_mn*media_wg_mulheres
    
    # Perda salarial do homem negro dada a empregabilidade - efeito composicao
    composicao_massa_emp_mn <- P1_mn*composicao_emp_mn*media_wg_mulheres
    
    # Massa Salarial Total Perdida (ou premiada) dos mulheres negras
    total_mn <-  discriminacao_massa_wg_mn + composicao_massa_wg_mn + 
      discriminacao_massa_emp_mn + composicao_massa_emp_mn
    
    anotri <- sprintf("%dT%d", aa, tri)
    bi <- 1000000000
    dt2 <- data.table(Ano_trimestre = anotri, 
                      total_mn = round(total_mn/bi, 2),
                      discriminacao_massa_wg_mn = round(discriminacao_massa_wg_mn/bi, 2),
                      composicao_massa_wg_mn = round(composicao_massa_wg_mn/bi, 2),
                      discriminacao_massa_emp_mn = round(discriminacao_massa_emp_mn/bi, 2),
                      composicao_massa_emp_mn = round(composicao_massa_emp_mn/bi, 2),
                      total_mb = round(total_mb/bi, 2),
                      composicao_massa_wg_mb = round(composicao_massa_wg_mb/bi, 2),
                      composicao_massa_emp_mb = round(composicao_massa_emp_mb/bi, 2))
    
    
    
    dt1 <- rbind(dt1, dt2, fill = TRUE)
    
  }
}

fwrite(dt1, file.path(csv_output, "resultados_massa_salarial_mulheres.csv"))