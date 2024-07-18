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

part_01 <- FALSE
part_02 <- FALSE

# Definindo usuário
#user <- "Rodrigo"
user <- "Alysson"

if (user == "Rodrigo") {
  # working folders
  datawork_folder <- file.path("C:/Users/Rodrigo/Documents/R/NERI_INSPER/conjuntura")
  github_folder <- file.path("C:/Users/Rodrigo/Documents/GitHub/conjuntura_discriminacao")
  one_drive_folder <- file.path("C:/Users/Rodrigo/Insper/Alysson Lorenzon Portella - conjuntura_pnadc")
  
}

if (user == "Alysson") {
  # working folders
  datawork_folder <- file.path("C:/Users/Rodrigo/Documents/R/NERI_INSPER/conjuntura")
  github_folder <- file.path("C:/Users/Alysson/Documents/@github/conjuntura_discriminacao")
  one_drive_folder <- file.path("C:/Users/Alysson/Documents/onedrive_alyssonlp1/@support_papers/@NERI/conjuntura_pnadc/conjuntura_discriminacao")
  
}


codes <- file.path(github_folder, "codes")
original_data <- file.path(one_drive_folder, "original_data")
intermediary_data <- file.path(one_drive_folder, "intermediary_data")
tables_output <- file.path(github_folder, "tables_output")
figures_output <- file.path(github_folder, "figures_output")
csv_files <- file.path(github_folder, "csv_files")
final_data <- file.path(one_drive_folder, "final_data")
temp_file <- file.path(one_drive_folder, "temp_file")


dir.create(intermediary_data)


list_objects_to_keep <- c("datawork_folder", "github_folder", "one_drive_folder",
                          "codes","tables_output", "figures_output",
                          "csv_files", "final_data", "ano_tri_fun")

# Codes --------------------
# Funções utilizadas em outros scripts:
source(file.path(codes, "0a_function_carta_conjuntura.R"))

# limpeza e estatistica descritiva
if (part_01) {
  source(file.path(codes, "1a_clean_data_carta_replicacao.R"))
  # source(file.path(codes, "1b_function_carta_conjuntura.R"))
  source(file.path(codes, "1c_resultados_carta_conjuntura.R"))
  source(file.path(codes, "1d_graficos_carta_conjuntura.R"))
  source(file.path(codes, "1e_graficos_carta_conjuntura_raca.R"))
  source(file.path(codes, "1f_graficos_carta_conjuntura_genero_raca.R"))
  source(file.path(codes, "1g_graficos_labor_recente_gph.R"))
  source(file.path(codes, "1h_frac_graficos_labor_recente.R"))
  source(file.path(codes, "1i_top_bottom_labor_recente.R"))
}

# modelo econometrico
if (part_02) {
  source(file.path(codes, "2a_hiatos.R"))
  source(file.path(codes, "2b_mensuracao_massa_salarial.R"))
  source(file.path(codes, "2c_massa_perdida_gph.R"))
  source(file.path(codes, "2d_penalidade_gph.R"))
}





