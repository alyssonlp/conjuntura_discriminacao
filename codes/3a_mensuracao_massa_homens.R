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
    
# HOMENS - SALARIOS
    
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
    position_homem <- which(dt[male == 1 & is.na(ln_r_hab_all) == F])
    
  # Homens Brancos - Contrafactual: Homens 
    
      media_wg_hb <- mean(exp(eq_wg_h$fitted.values[position_hb] + 
                                     eq_wg_h$residual[position_hb]))
      
      media_wg_homens <- mean(exp(eq_wg_h$fitted.values[position_homem] + 
                                     eq_wg_h$residual[position_homem]))
      
       # Obs. homens brancos nao tem efeito discriminacao
    
    # Homens brancos na economia (P_0)
      P0_hb <- dt[gender_race == "Homem Branco", sum(V1028)]
    
    # Homens brancos empregados (e_0)
      e0_hb <- dt[gender_race == "Homem Branco",wtd.mean(pea_emp, weights = V1028)]
      
    # Diferença na composicao
      composicao_wg_hb <- media_wg_hb - media_wg_homens
      
    # Massa salarial perdida devido ao efeito composicao
        composicao_massa_wg_hb <- P0_hb*composicao_wg_hb*e0_hb
    # Massa salarial total perdida(premiada) homens brancos
        wg_massa_perdida_hb <- composicao_massa_wg_hb
        
  # Homens Negros - Contrafactual: Homens
        # Homens negros na economia (P_1)
        P1_hn <- dt[gender_race == "Homem Negro", sum(V1028)]
        
        # Homens negros empregados (e_1)
        e1_hn <- dt[gender_race == "Homem Negro",wtd.mean(pea_emp, weights = V1028)]
        
        
      media_wg_hn <- mean(exp(eq_wg_h$fitted.values[position_hn] + 
                                      eq_wg_h$residual[position_hn]))
        
      media_wg_hn_sem_discr <- mean(exp(eq_wg_h$fitted.values[position_hn] + 
                                    (-1)*eq_wg_h$coefficients[2] +
                                    eq_wg_h$residual[position_hn]))
    
  # Componente discriminatório (gama)
  discriminacao_wg_hn <-  media_wg_hn - media_wg_hn_sem_discr 
  
  # Diferença na composicao
  composicao_wg_hn <- media_wg_negros_sem_discr - media_wg_homens
  
  # Penalidade salarial devido a composicao e a discriminacao
  penalidade_wg_hn <- discriminacao_wg_hn + composicao_wg_hn
  
  # Massa Salarial  perdida dos Homens Negros
    wg_massa_perdida_hn <- P1_hn*penalidade_wg_hn*e1_hn
  
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
pos_homem_emp <- which(dt[male == 1])

  # Homens Brancos - Contrafactual: Homens 

  media_emp_hb <- mean(eq_emp_h$fitted.values[pos_hb_emp] + 
                        eq_emp_h$residual[pos_hb_emp])
  
  media_emp_homens <- mean(eq_emp_h$fitted.values[pos_homem_emp] + 
                         eq_emp_h$residual[pos_homem_emp])
  
  # Efeito discriminacao == 0
  # Diferença na composicao
  composicao_emp_hb <- media_emp_hb - media_emp_homens
  
  # Perda salarial do homem branco dada a empregabilidade
  emp_massa_perdida_hb <- P0_hb*composicao_wg_hb*media_wg_homens
  
  # Perda salarial do homem branco dada a empregabilidade - efeito composicao
  composicao_massa_emp_hb <- emp_massa_perdida_hb
  
  # Massa Salarial Total Perdida (ou premiada) dos homens brancos
  total_hb <- composicao_massa_wg_hb + composicao_massa_emp_hb
  
  # Homens Negros - Contrafactual: Homens 
  
  media_emp_hn <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
                         eq_emp_h$residual[pos_hn_emp])
  
  media_emp_hn_sem_discr <- mean(eq_emp_h$fitted.values[pos_hn_emp] + 
                                   (-1)*eq_emp_h$coefficients[2] + eq_emp_h$residual[pos_hn_emp])
  
  media_emp_homens <- mean(eq_emp_h$fitted.values[pos_homem_emp] + 
                             eq_emp_h$residual[pos_homem_emp])
  
  # Componente discriminatorio
  discriminacao_emp_hn <- media_emp_hn - media_emp_hn_sem_discr
  
  # Diferença na composicao
  composicao_emp_hn <- media_emp_hn_sem_discr - media_emp_homens
  
  # penalidade na empregaabilidade - homem negro
  penalidade_emp_hn <- discriminacao_emp_hn + composicao_emp_hn
  
  # Perda salarial do homem negro dada a empregabilidade
  emp_massa_perdida_hb <- P1_hn*penalidade_emp_hn*media_wg_homens
  
  # Perda salarial do homem negro dada a empregabilidade - efeito discriminacao
  discriminacao_massa_emp_hn <- P1_hn*discriminacao_emp_hn*media_wg_homens
  
  # Perda salarial do homem negro dada a empregabilidade - efeito composicao
  composicao_massa_emp_hn <- P1_hn*discriminacao_massa_emp_hn*media_wg_homens
  
  # Massa Salarial Total Perdida (ou premiada) dos homens negros
  total_hn <-  discriminacao_massa_wg_hn + composicao_massa_wg_hn + 
    discriminacao_massa_emp_hn + composicao_massa_emp_hn
  }
}
  

  