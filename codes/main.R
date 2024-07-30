# Carregando pacotes
library(PNADcIBGE)
library(survey)
library(dplyr)
library(data.table)
library(questionr)
library(ggplot2)
library(fixest)
library(dineq)
library(tidyr)
library(knitr)
library(gt)
library(webshot2)
library(kableExtra)



part_01 <- FALSE
part_02 <- FALSE
part_03 <- FALSE

# Definindo usuário
user <- "Rodrigo"
#user <- "Alysson"
#user <- "Alysson_cpp"


if (user == "Rodrigo") {
  # working folders
  github_folder <- file.path("C:/Users/Rodrigo/Documents/GitHub/conjuntura_discriminacao")
  one_drive_folder <- file.path("C:/Users/Rodrigo/Insper/Alysson Lorenzon Portella - conjuntura_pnadc/conjuntura_discriminacao")
  
}

if (user == "Alysson") {
  # working folders
  github_folder <- file.path("C:/Users/Alysson/Documents/@github/conjuntura_discriminacao")
  one_drive_folder <- file.path("C:/Users/Alysson/Documents/onedrive_alyssonlp1/@support_papers/@NERI/conjuntura_pnadc/conjuntura_discriminacao")
  
}

if (user == "Alysson_cpp") {
  # working folders
  github_folder <- file.path("C:/Users/alyssonlp1/Documents/@github/conjuntura_discriminacao")
  one_drive_folder <- file.path("C:/Users/alyssonlp1/OneDrive - Insper/@support_papers/@NERI/conjuntura_pnadc/conjuntura_discriminacao")
  
}

codes <- file.path(github_folder, "codes")
original_data <- file.path(one_drive_folder, "original_data")
intermediary_data <- file.path(one_drive_folder, "intermediary_data")
outputs <- file.path(github_folder, "outputs")
tables_output <- file.path(outputs, "tables_output")
figures_output <- file.path(outputs, "figures_output")
csv_output <- file.path(outputs, "csv_output")
final_data <- file.path(one_drive_folder, "final_data")
temp_file <- file.path(one_drive_folder, "temp_file")


#dir.create(intermediary_data)
#dir.create(final_data)
#dir.create(temp_file)


list_objects_to_keep <- c("datawork_folder", "github_folder", "one_drive_folder",
                          "codes", "outputs" ,"tables_output", "figures_output",
                          "csv_output", "intermediary_data", "final_data", 
                          "ano_tri_fun", "part_01", "part_02", "part_03",
                          "dist_fun", "list_objects_to_keep")

# Codes --------------------
# Funções utilizadas em outros scripts:
source(file.path(codes, "0a_function_carta_conjuntura.R"))

# limpeza e estatistica descritiva
if (part_01) {
  source(file.path(codes, "1a_clean_data_carta_replicacao.R"))
  source(file.path(codes, "1b_resultados_carta_conjuntura.R"))
  source(file.path(codes, "1c_graficos_carta_conjuntura_raca.R"))
  source(file.path(codes, "1d_graficos_carta_conjuntura_genero_raca.R"))
  source(file.path(codes, "1e_graficos_labor_recente_gph.R"))
  source(file.path(codes, "1f_pea_adult.R"))
  source(file.path(codes, "1g_frac_graficos_labor_recente.R"))
  source(file.path(codes, "1h_top_bottom_labor_recente.R"))
  source(file.path(codes, "1i_top_bottom.R"))
  source(file.path(codes, "1j_top_bottom_gph_linha.R"))
  source(file.path(codes, "1k_top_bottom_ppb.R"))
  source(file.path(codes, "1l_top_botoom_ppb_gph.R"))
  source(file.path(codes, "1m_topo_bottom_ppb_linha_gph.R"))
}

# modelo econometrico
if (part_02) {
  source(file.path(codes, "2a_mensuracao_massa_salarial.R"))
  source(file.path(codes, "2b_massa_perdida_gph.R"))
  source(file.path(codes, "2c_massa_perdida_negros_gph.R"))
  source(file.path(codes, "2d_tabelas_resultados_massa_e_individual.Rmd"))
}

# contrafactual Brasil
if (part_03) {
  source(file.path(codes, "3a_mensuracao_massa_homens.R"))
  source(file.path(codes, "3b_mensuracao_massa_mulheres.R"))
  source(file.path(codes, "3c_massa_contrafacutal_br_gph.R"))
}




