compute_percents <- function(aa, tri) {
    
  print(paste0("Computing shares for year ", aa, ", quarter ", tri)) 
  
  rds_file <- sprintf("pnadc%d_%d_carta.rds", aa, tri)
  dt <- readRDS(file.path(intermediary_data, rds_file))
  
  # Calculando os percentis
  top10 <- quantile(dt$r_hab_all, 0.90, na.rm = TRUE)
  top5 <- quantile(dt$r_hab_all, 0.95, na.rm = TRUE)
  top1 <- quantile(dt$r_hab_all, 0.99, na.rm = TRUE)
  bottom10 <- quantile(dt$r_hab_all, 0.10, na.rm = TRUE)
  bottom5 <- quantile(dt$r_hab_all, 0.05, na.rm = TRUE)
  bottom1 <- quantile(dt$r_hab_all, 0.01, na.rm = TRUE)
  
  # Criando as variáveis dummies
  dt[, top10 := as.numeric(r_hab_all >= top10)]
  dt[, top5 := as.numeric(r_hab_all >= top5)]
  dt[, top1 := as.numeric(r_hab_all >= top1)]
  dt[, bottom10 := as.numeric(r_hab_all <= bottom10)]
  dt[, bottom5 := as.numeric(r_hab_all <= bottom5)]
  dt[, bottom1 := as.numeric(r_hab_all <= bottom1)]
  
  # Calculando as médias ponderadas
  distribuicao <- dt[, .(
    top10 = wtd.mean(100*top10, weights = V1028, na.rm = TRUE),
    top5 = wtd.mean(100*top5, weights = V1028, na.rm = TRUE),
    top1 = wtd.mean(100*top1, weights = V1028, na.rm = TRUE),
    bottom10 = wtd.mean(100*bottom10, weights = V1028, na.rm = TRUE),
    bottom5 = wtd.mean(100*bottom5, weights = V1028, na.rm = TRUE),
    bottom1 = wtd.mean(100*bottom1, weights = V1028, na.rm = TRUE)
  ), by = raca]
  
  tb <- as.data.table(distribuicao)
  
  tb[, t10 := (top10 / sum(top10)) * 100]
  tb[, t5 := (top5 / sum(top5)) * 100]
  tb[, t1 := (top1 / sum(top1)) * 100]
  tb[, b10 := (bottom10 / sum(bottom10)) * 100]
  tb[, b5 := (bottom5 / sum(bottom5)) * 100]
  tb[, b1 := (bottom1 / sum(bottom1)) * 100]
  
  anotri <- sprintf("%dT%d", aa, tri)
  tb[, Ano_trimestre := anotri]
  
  tb <- tb[, .(raca, Ano_trimestre, t10, t5, t1, b10, b5, b1)]
  
  return(tb)
}

# Iterando por todos os anos e trimestres desde 2012
anos <- 2012:2024
trimestres <- 1:4

resultado <- rbindlist(
  lapply(anos, function(ano) {
    if (ano == 2024) {
      return(compute_percents(aa = ano, tri = 1))
    } else {
      return(rbindlist(
        lapply(trimestres, function(tri) {
          compute_percents(aa = ano, tri = tri)
        })
      ))
    }
  })
)

fwrite(resultado, file.path(csv_output, "top_bottom_ppb.csv"))
