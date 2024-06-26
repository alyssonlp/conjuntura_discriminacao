
# gph 1 - rendimento habitual médio
# gph 2 - rendimento habitual e efetivo medio mensal (dessazonalizado)
# gph 4 - massa salarial real habitual e efetiva
# gph 5 - taxa de variacao interanual da massa salarial real habitual e efetiva
# gph 6 - índice de gini - rendimento habitual de todos os trabalhos - renda individual e domicilIar

# calcular para todos, depois por recorte 1: raca, recorte 2: raca e genero

ano_tri_fun <- function(ano, trimestre) {
  
  rds_name <- paste0("pnadc", ano, "_", trimestre, "_carta.rds")
  rds_file <- readRDS(file.path(original_data, rds_name))
  
  resultados_br <-
    rds_file[, list(renda_media_hab = wtd.mean(r_hab_all,weights =  V1028),
                  renda_media_efe = wtd.mean(r_efe_all, weights = V1028),
                  massa_hab = sum(r_hab_all*V1028, na.rm = TRUE),
                  massa_efe = sum(r_efe_all*V1028, na.rm = TRUE),
                  tx_desocup = wtd.mean(unemp, weights = V1028),
                  pea_fac = wtd.mean(pea, weights = V1028),
                  gini_hab = gini.wtd(r_hab_all, weights = V1028),
                  gini_efe = gini.wtd(r_efe_all, weights = V1028))]
  
  resultados_raca <-
    rds_file[, list(renda_media_hab = wtd.mean(r_hab_all,weights =  V1028),
                    renda_media_efe = wtd.mean(r_efe_all, weights = V1028),
                    massa_hab = sum(r_hab_all*V1028, na.rm = TRUE),
                    massa_efe = sum(r_efe_all*V1028, na.rm = TRUE),
                    tx_desocup = wtd.mean(unemp, weights = V1028),
                    pea_fac = wtd.mean(pea, weights = V1028),
                    gini_hab = gini.wtd(r_hab_all, weights = V1028),
                    gini_efe = gini.wtd(r_efe_all, weights = V1028)),
             by = c("nonwhite")]
  
  resultados_gen_raca <-
    rds_file[, list(renda_media_hab = wtd.mean(r_hab_all,weights =  V1028),
                    renda_media_efe = wtd.mean(r_efe_all, weights = V1028),
                    massa_hab = sum(r_hab_all*V1028, na.rm = TRUE),
                    massa_efe = sum(r_efe_all*V1028, na.rm = TRUE),
                    tx_desocup = wtd.mean(unemp, weights = V1028),
                    pea_fac = wtd.mean(pea, weights = V1028),
                    gini_hab = gini.wtd(r_hab_all, weights = V1028),
                    gini_efe = gini.wtd(r_efe_all, weights = V1028)), 
             by = c("gender_race")]
  
  return(list(resultados_br = resultados_br,
              resultados_raca = resultados_raca,
              resultados_gen_raca = resultados_gen_raca))
  
  
}

