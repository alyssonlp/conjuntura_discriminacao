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
    dt <- setDT(dt)[V2009 >= 25 & V2009 <= 65]
    
# HOMENS - SALARIOS
    
    eq_wg_h <- lm(ln_r_hab_all ~ nonwhite + educ + educ_sq + potential_xp + 
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
                  data = dt[male == 1 & is.na(ln_r_hab_all) == F])
    
    pos_hn <- which(as.data.table(eq_wg_h[["model"]])[, nonwhite] == 1)
    pos_hb <- which(as.data.table(eq_wg_h[["model"]])[, nonwhite] == 0)
    pos_h <-  which(as.data.table(eq_wg_h[["model"]])[, nonwhite] == 0 |
                      as.data.table(eq_wg_h[["model"]])[, nonwhite] == 1)
    
    pos_dt_hn <- which(dt[, male == 1 & is.na(r_hab_all) == F & nonwhite == 1])
    pos_dt_hb <- which(dt[, male == 1 & is.na(r_hab_all) == F & nonwhite == 0])
    pos_dt_h <- which(dt[, male == 1 & is.na(r_hab_all) == F])

  #### Homens Brancos SALARIOS - Contrafactual: Homens ----
    
      media_wg_hb <- mean(exp(eq_wg_h$fitted.values[pos_hb] +  eq_wg_h$residual[pos_hb]),
                          weights = dt[pos_dt_hb, V1028])
      
      media_wg_homens <- mean(exp(eq_wg_h$fitted.values[pos_h] + eq_wg_h$residual[pos_h]),
                              weights = dt[pos_dt_h, V1028])
      
       # Obs. homens brancos nao tem efeito discriminacao
    
    # Homens brancos na economia (P_0)
      P0_hb <- dt[male == 1 & nonwhite == 0, sum(V1028)]
    
    # Homens brancos empregados (e_0)
      e0_hb <- dt[male == 1 & nonwhite == 0, wtd.mean(pea_emp, weights = V1028)]
      
    # Diferença na composicao
      composicao_wg_hb <- media_wg_hb - media_wg_homens
      
    # Massa salarial perdida devido ao efeito composicao
        composicao_massa_wg_hb <- P0_hb*composicao_wg_hb*e0_hb
        
    # Massa salarial total perdida(premiada) homens brancos
        wg_massa_perdida_hb <- composicao_massa_wg_hb + 0
        
    #### Homens Negros SALARIOS - Contrafactual: Homens ----
        # Homens negros na economia (P_1)
        P1_hn <- dt[male == 1 & nonwhite == 1, sum(V1028)]
        
        # Homens negros empregados (e_1)
        e1_hn <- dt[male == 1 & nonwhite == 1, wtd.mean(pea_emp, weights = V1028)]
        
        
      media_wg_hn <- mean(exp(eq_wg_h$fitted.values[pos_hn] + eq_wg_h$residual[pos_hn]),
                          weights = dt[pos_dt_hn, V1028])
        
      media_wg_hn_sem_discr <- mean(exp(eq_wg_h$fitted.values[pos_hn] + 
                                    (-1)*eq_wg_h$coefficients[2] +
                                    eq_wg_h$residual[pos_hn]),
                                    weights = dt[pos_dt_hn, V1028])
    
  # Componente discriminatório 
  discriminacao_wg_hn <-  media_wg_hn - media_wg_hn_sem_discr 
  
  # Diferença na composicao
  composicao_wg_hn <- media_wg_hn_sem_discr - media_wg_homens
  
  # Penalidade salarial devido a composicao e a discriminacao
  penalidade_wg_hn <- discriminacao_wg_hn + composicao_wg_hn
  
  # Massa Salarial  perdida dos Homens Negros
    wg_massa_perdida_hn <- P1_hn*penalidade_wg_hn*e1_hn
  
  # Massa salarial perdida devido ao efeito composicao
    composicao_massa_wg_hn <- P1_hn*composicao_wg_hn*e1_hn
  
  # Massa salarial perdida devido ao efeito discriminacao
    discriminacao_massa_wg_hn <- P1_hn*discriminacao_wg_hn*e1_hn
    
    
    #### EMPREGABILIDADE -----
  # Equação Probabilidade de estar empregado - homens
    
eq_emp_h <- lm(pea_emp ~ nonwhite + parents + educ + educ_sq + potential_xp + 
               potential_xp_sq + married + rural + metropolitan + 
               estudando + factor(UF), 
               weights = V1028, 
               data = dt[male == 1,])
    
    pos_hn_emp <- which(as.data.table(eq_emp_h[["model"]])[, nonwhite] == 1)
    pos_hb_emp <- which(as.data.table(eq_emp_h[["model"]])[, nonwhite] == 0)
    pos_h_emp  <- which(as.data.table(eq_emp_h[["model"]])[, nonwhite] == 0 |
                          as.data.table(eq_emp_h[["model"]])[, nonwhite] == 1)
    
    pos_dt_hn_emp <- which(dt[, male == 1 & nonwhite == 1])
    pos_dt_hb_emp <- which(dt[, male == 1 & nonwhite == 0])
    pos_dt_h_emp  <- which(dt[, male == 1])


  #### Homens Brancos EMPREGO - Contrafactual: Homens ----

  media_emp_hb <- mean(eq_emp_h$fitted.values[pos_hb_emp] + 
                        eq_emp_h$residual[pos_hb_emp],
                       weights = dt[pos_dt_hb_emp, V1028])
  
  media_emp_homens <- mean(eq_emp_h$fitted.values[pos_h_emp] + 
                         eq_emp_h$residual[pos_h_emp],
                         weights = dt[pos_dt_h_emp, V1028])
  
  # Efeito discriminacao == 0
  # Diferença na composicao
  composicao_emp_hb <- media_emp_hb - media_emp_homens
  
  # Perda salarial do homem branco dada a empregabilidade
  emp_massa_perdida_hb <- P0_hb*composicao_emp_hb*media_wg_homens
  
  # Perda salarial do homem branco dada a empregabilidade - efeito composicao
  composicao_massa_emp_hb <- emp_massa_perdida_hb + 0
  
  # Massa Salarial Total Perdida (ou premiada) dos homens brancos
  total_hb <- composicao_massa_wg_hb + composicao_massa_emp_hb
  
  #### Homens Negros EMPREGO - Contrafactual: Homens ----
  
  media_emp_hn <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
                         eq_emp_h$residual[pos_hn_emp],
                       weights = dt[pos_dt_hn_emp, V1028])
  
  media_emp_hn_sem_discr <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
                                   (-1)*eq_emp_h$coefficients[2] + 
                                   eq_emp_h$residual[pos_hn_emp],
                                 weights = dt[pos_dt_hn_emp, V1028])
  
  # Componente discriminatorio
  discriminacao_emp_hn <- media_emp_hn - media_emp_hn_sem_discr
  
  # Diferença na composicao
  composicao_emp_hn <- media_emp_hn_sem_discr - media_emp_homens
  
  # penalidade na empregabilidade - homem negro
  penalidade_emp_hn <- discriminacao_emp_hn + composicao_emp_hn
  
  # Perda salarial do homem negro dada a empregabilidade
  emp_massa_perdida_hn <- P1_hn*penalidade_emp_hn*media_wg_homens
  
  # Perda salarial do homem negro dada a empregabilidade - efeito discriminacao
  discriminacao_massa_emp_hn <- P1_hn*discriminacao_emp_hn*media_wg_homens
  
  # Perda salarial do homem negro dada a empregabilidade - efeito composicao
  composicao_massa_emp_hn <- P1_hn*composicao_emp_hn*media_wg_homens
  
  # Massa Salarial Total Perdida (ou premiada) dos homens negros
  total_hn <-  discriminacao_massa_wg_hn + composicao_massa_wg_hn + 
    discriminacao_massa_emp_hn + composicao_massa_emp_hn
  
  anotri <- sprintf("%dT%d", aa, tri)
  bi <- 1000000000
  dt2 <- data.table(Ano_trimestre = anotri, 
                    total_hn = round(total_hn/bi, 2),
                    discriminacao_massa_wg_hn = round(discriminacao_massa_wg_hn/bi, 2),
                    composicao_massa_wg_hn = round(composicao_massa_wg_hn/bi, 2),
                    discriminacao_massa_emp_hn = round(discriminacao_massa_emp_hn/bi, 2),
                    composicao_massa_emp_hn = round(composicao_massa_emp_hn/bi, 2),
                    total_hb = round(total_hb/bi, 2),
                    composicao_massa_wg_hb = round(composicao_massa_wg_hb/bi, 2),
                    composicao_massa_emp_hb = round(composicao_massa_emp_hb/bi, 2))
           

  
  dt1 <- rbind(dt1, dt2, fill = TRUE)
  
  }
}

fwrite(dt1, file.path(csv_output, "resultados_massa_salarial_homens.csv"))
  

  