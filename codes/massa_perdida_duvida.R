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
    
    aa = 2024
    tri = 1
    rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
    dt <- readRDS((file.path(original_data, rds_file)))


homens <- dt[male == 1,]
homens <- setDT(homens)[is.na(ln_r_hab_all) == F]
eq_wg_h <- lm(ln_r_hab_all ~ nonwhite + educ + educ_sq + potential_xp + potential_xp_sq + rural + metropolitan + 
                diretores_gerentes + ciencias_intelectuais + tec_nivel_medio + apoio_adm + 
                servicos_comercio + agropecuaria_pesca + operarios_construcao + maquinas_montadores + 
                ocup_elementares + ffaa + private_formal + private_informal + domestic_formal + 
                domestic_informal + public_service + self_employed + employer + agricultura + industria + 
                construcao + comercio + servios_profissionais + transporte + servicos_pessoais_coletivos + 
                adm_publica + educ_saude + RO + AC + AM + RR + PA + AP + TO + MA + PI + CE + RN + PB + PE + AL + 
                SE + BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO, data = homens)

# calculando a média esperada das covariadas - homens
media_negros <- mean(exp(eq_wg_h$fitted.values[which(homens$nonwhite == 1)] + eq_wg_h$residual[which(homens$nonwhite == 1)]))
media_negros_sem_disc <- mean(exp(eq_wg_h$fitted.values[which(homens$nonwhite == 1)] + (-1)*eq_wg_h$coefficients[2] + eq_wg_h$residual[which(homens$nonwhite == 1)]))
media_brancos <- mean(exp(eq_wg_h$fitted.values[which(homens$nonwhite == 0)] + eq_wg_h$residual[which(homens$nonwhite == 0)]))

# Delta X - homens
delta_x_homem_simples <-  media_negros_sem_disc - media_negros

# Beta - homens
beta_homem <- eq_wg_h$coefficients

# Gama - homem
gama_homem <- exp(eq_wg_h$coefficients[2])*(-1)

# Massa Salarial (nao levando em conta a empregabilidade):N*salario_medio_brancos + N_1 (Delta_X * Beta + gama)
# Delta_X = E[X|R=1] - e[x|R=0]
# O delta_x_homem_simples, não seria, <- media_negros - media_brancos?
# Matriz Beta tem dimensao 60x1. Delta_x, se for matriz, tem dimensao 1x1. 
# Multiplicando, Delta_X_Beta terá dimensao 60x1. Deveria ter dimensao 1x1 para inserir na equacao da linha 27, 
#de forma a termos um unico valor de massa salarial (1x1). 

# Para Mulheres
mulheres <- dt[male == 0,]
mulheres <- setDT(mulheres)[is.na(ln_r_hab_all) == F]
eq_wg_m <- lm(ln_r_hab_all ~ nonwhite + educ + educ_sq + potential_xp + potential_xp_sq + rural + metropolitan + 
                diretores_gerentes + ciencias_intelectuais + tec_nivel_medio + apoio_adm + 
                servicos_comercio + agropecuaria_pesca + operarios_construcao + maquinas_montadores + 
                ocup_elementares + ffaa + private_formal + private_informal + domestic_formal + 
                domestic_informal + public_service + self_employed + employer + agricultura + industria + 
                construcao + comercio + servios_profissionais + transporte + servicos_pessoais_coletivos + 
                adm_publica + educ_saude + RO + AC + AM + RR + PA + AP + TO + MA + PI + CE + RN + PB + PE + AL + 
                SE + BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO, data = mulheres)

# calculando a média esperada das covariadas - homens
media_negras <- mean(exp(eq_wg_m$fitted.values[which(mulheres$nonwhite == 1)] + eq_wg_m$residual[which(mulheres$nonwhite == 1)]))
media_negras_sem_disc <- mean(exp(eq_wg_m$fitted.values[which(mulheres$nonwhite == 1)] + (-1)*eq_wg_m$coefficients[2] + eq_wg_m$residual[which(mulheres$nonwhite == 1)]))
media_brancas <- mean(exp(eq_wg_m$fitted.values[which(mulheres$nonwhite == 0)] + eq_wg_m$residual[which(mulheres$nonwhite == 0)]))

# Delta X - mulheres
delta_x_mulher_simples <-  media_negras_sem_disc - media_negras

# Beta - homens
beta_mulher <- eq_wg_m$coefficients

# Gama - mulheres
gama_mulher <- exp(eq_wg_m$coefficients[2])*(-1)

  }
}