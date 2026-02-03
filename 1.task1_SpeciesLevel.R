##en este script se crea la categoria Species_level en task 1 donde aparece 
##el species group si son los grupos menores o la especie en las principales

library(dplyr)
library(readxl)
        
setwd("C:/")
task1 <- read_excel("task1.xlsx")

task1 <- task1 %>%
  mutate(Species_level = case_when(
    SpeciesGrp == "1-Tuna (major sp.)" ~ Species,
    SpeciesGrp == "2-Tuna (small)" ~ "Small Tuna",
    SpeciesGrp == "3-Tuna (other)" ~ "Other Tuna and tuna like species",
    SpeciesGrp == "4-Sharks (major)" ~ Species,
    SpeciesGrp == "5-Sharks (other)" ~ "Other sharks",
    SpeciesGrp == "6-Other Species" ~ "Other species",
    TRUE ~ NA_character_  # Valor por defecto para los casos que no cumplen
  ))


# Cargar el paquete
library(writexl)

# Guardar el dataframe en Excel
write_xlsx(task1, "task1_con_species_level.xlsx")
