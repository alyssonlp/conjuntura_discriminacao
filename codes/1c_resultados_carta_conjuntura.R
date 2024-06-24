ano <- c( 2016)
trimestre <- c(1:4)
 
for (aa in ano) {
  for (tri in trimestre) {
    carta <- ano_tri_fun(aa, tri) 
    
    resultados_brasil <- sprintf("resultados_brasil_%d_%d.csv", ano, tri)
    carta_brasil <- write.csv(carta$resultados_br, file.path(csv_files, resultados_brasil))
    
    resultados_raca <- sprintf("resultados_raca_%d_%d.csv", ano, tri)
    carta_raca <- write.csv(carta$resultados_raca, file.path(csv_files, resultados_raca))
    
    resultados_gen_raca <- sprintf("resultados_genero_raca_%d_%d.csv", ano, tri)
    carta_gen_raca <- write.csv(carta$resultados_gen_raca, file.path(csv_files, resultados_gen_raca))
  }
}

