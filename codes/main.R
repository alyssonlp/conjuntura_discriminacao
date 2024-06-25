# Carregando pacotes
library(PNADcIBGE)
library(survey)
library(dplyr)
library(data.table)
library(questionr)
library(ggplot2)
library(fixest)
library(dineq)

part_01 <- FALSE
part_02 <- FALSE
part_03 <- FALSE 

# Definindo usuário
user <- "Rodrigo"

if (user == "Rodrigo") {
  # working folders
  datawork_folder <- file.path("C:/Users/Rodrigo/Documents/R/NERI_INSPER/conjuntura")
  github_folder <- file.path("C:/Users/Rodrigo/Documents/GitHub/Convergencia")
  
}

codes <- file.path(datawork_folder, "codes")
intermediary_data <- file.path(datawork_folder, "intermediary_data")
tables_output <- file.path(datawork_folder, "tables_output")
figures_output <- file.path(datawork_folder, "figures_output")
csv_files <- file.path(datawork_folder, "csv_files")
final_data <- file.path(datawork_folder, "final_data")

list_objects_to_keep <- c("datawork_folder", "github_folder", "codes",
                          "tables_output", "figures_output",
                          "csv_files", "final_data")

if (part_01 == TRUE ){
   source(file.path(codes, "1a_clean_data_carta_replicacao.R"))
   source(file.path(codes, "1b_function_carta_conjuntura.R"))
   source(file.path(codes, "1c_resultados_carta_conjuntura.R"))
   source(file.path(codes, "1d_graficos_carta_conjuntura.R"))
   source(file.path(codes, "1e_graficos_carta_conjuntura_raca.R"))
   source(file.path(codes, "1f_graficos_carta_conjuntura_genero_raca.R"))
}

if (part_02 == TRUE ){
  # amostra 25-54 anos
  source(file.path(codes, "2a_clean_data_conjuntura.R"))
  source(file.path(codes, "2b_rendimento_medio_conjuntura.R"))
  source(file.path(codes, "2c_modelos.R"))
}

if (part_03 == TRUE ){
source(file.path(codes, "3a_modelos.R"))
}

# Funções
source(file.path(codes, "1b_function_carta_conjuntura.R"))