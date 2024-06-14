# Carregando pacotes
library(PNADcIBGE)
library(survey)
library(dplyr)
library(data.table)
library(questionr)


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