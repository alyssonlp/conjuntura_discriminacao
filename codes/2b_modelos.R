# criando vetores para automatizar os arquivos conforme o ano e o trimestre
ano <- c(2024)
trimestre <- c(1)

# no sprintf %d sao para valores inteiros e %s para nomes(string)
# isso é similar ao `' do stata no looping

for(aa in ano) {
  for (tri in trimestre) {
    rds_file <- sprintf("pnadc%d_%d.rds", aa, tri)
    dt <- readRDS((file.path(intermediary_data, rds_file)))
    
# equacao salarial habitual - todos os trabalhos
    
    eq_salarial <- glm(ln_r_hab_all ~ educ + educ_sq + educ_cubic + educ_forth +
                         potential_xp + married_woman + rural + metropolitan +
                         private_formal + private_informal + domestic_formal +
                         domestic_informal + public_service + self_employed  +
                         employer + agricultura + industria + construcao + 
                         comercio + servios_profissionais + transporte + 
                         servicos_pessoais_coletivos + adm_publica +
                         educ_saude + nonwhite + RO + AC + AM + RR + PA 
                         + AP + TO + MA + PI + CE + RN + PB + PE + AL + SE + 
                         BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT + GO + DF, 
                       data = dt, weights = V1028)
 
    summary(eq_salarial)   
    
# equação empregabilidade - probabilidade em estar desempregado
    
    eq_empregabilidade <- glm(unemp ~ educ + educ_sq + educ_cubic + educ_forth + 
                                potential_xp + married_woman + rural + metropolitan +
                                mother + RO + AC + AM + RR + PA + AP + TO + MA + 
                                PI + CE + RN + PB + PE + AL + SE + 
                                BA + MG + ES + RJ + SP + PR + SC + RS + MS + MT +
                                GO + DF , data = dt, weights = V1028)
    
    summary(eq_empregabilidade)
  }
}


