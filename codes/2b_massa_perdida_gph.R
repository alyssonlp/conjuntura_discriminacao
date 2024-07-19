# Criando gph da massa salarial perdida 

mass <- fread(file.path(csv_output, "resultados_massa_salarial.csv"))

# Gph para homens negros
# total, composicao wg, discriminacao wg, composicao emp, discriminacao emp
# Transformar os dados para o formato longo
homem <- mass[, .(Ano_trimestre, total_perdido_hn, massa_wg_composicao_hn, massa_wg_discriminacao_hn,
                  massa_emp_composicao_hn, massa_emp_discriminacao_hn)]

h_long <- melt(homem, id.vars = "Ano_trimestre", 
                variable.name = "Tipo", 
                value.name = "Valor")

