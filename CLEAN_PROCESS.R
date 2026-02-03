### ESTE SCRIPT TIENE DOS PARTES: UNA LIMPIEZA DE NOMENCLATURAS QUE NO SIRVEN 
#PARA EL ANALISIS POSTERIOR
#LA ELIMINACION DE PUNTOS QUE SE DETECTARON MAL, A PARTIR DEL CALCULO DE PROPORCION DE ESPECIES

#=========PRIMERA PARTE: LIMPIEZA ================
library(dplyr)

setwd("C:")
catdis <- read.csv2("CATDIS_clean.csv")
# Eliminamos las filas donde fleetCode contenga "MIX" O "NEI"
catdis_clean <- catdis %>% filter(!grepl("MIX|NEI", FleetCode))
write.csv2(catdis_clean, "CATDIS_clean.csv")

TASK1 <- read.csv2("TASK1_fleetmerge.csv")
# Eliminamos las filas donde fleetCode contenga "MIX" O "NEI"
TASK1_clean <- TASK1 %>% filter(!grepl("MIX|NEI", FleetCode))
write.csv2(TASK1_clean, "TASK1_clean.csv")


#===========SEGUNDA PARTE
# Código para calcular proporciones de especies por flota y coordenadas (desde 2014)

# YA TENEMOS EL FILTRO TEMPORAL EN CATDIS1, PERO POR SI ACASO LO APLICAMOS DE NUEVO:

setwd("C:/")
CATDIS <- read.csv2("CATDIS_SBT_ECOR.csv")
names(CATDIS)[names(CATDIS)=="Gear_Fleet"]<-"Fleet"
names(CATDIS)[names(CATDIS)=="Lon3"]<-"xLon5ctoid"
names(CATDIS)[names(CATDIS)=="Lat3"]<-"yLat5ctoid"

CATDIS$...1 <- NULL

# Asegurarnos de que solo usamos datos desde 2014
CATDIS_filtered <- CATDIS %>% 
  select(SpeciesCode, YearC, FleetCode, GearGrp, yLat5ctoid, xLon5ctoid, Catch_t, Fleet) %>%
  filter(YearC >= 2014) %>%
  group_by(yLat5ctoid, xLon5ctoid, SpeciesCode, YearC, Fleet) %>%
  summarise(Catch_t = sum(Catch_t), .groups = 'drop')

# Preparar datos de composición de especies (solo desde 2014)
Fleets_catch_lon_lat_species <- CATDIS_filtered %>%
  group_by(yLat5ctoid, xLon5ctoid, Fleet, SpeciesCode) %>%
  summarize(meancatch = mean(Catch_t), .groups = 'drop')

# Calcular proporciones por especie, flota Y coordenadas (solo datos 2014-2023)
Fleets_catch_composition <- Fleets_catch_lon_lat_species %>%
  group_by(Fleet, yLat5ctoid, xLon5ctoid, SpeciesCode) %>%  # Agrupamos por flota, lat, lon y especie
  summarize(
    total_catch = sum(meancatch, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Fleet, yLat5ctoid, xLon5ctoid) %>%  # Agrupamos por flota y coordenadas para calcular proporciones
  mutate(
    cell_total_catch = sum(total_catch, na.rm = TRUE),
    proportion = ifelse(cell_total_catch > 0, total_catch / cell_total_catch, 0)
  ) %>%
  ungroup()


# Crear tabla wide con las proporciones - versión con columnas separadas para lat/lon
Species_proportion_table_grid <- Fleets_catch_composition %>%
  select(Fleet, yLat5ctoid, xLon5ctoid, SpeciesCode, proportion) %>%
  pivot_wider(
    names_from = SpeciesCode,
    values_from = proportion,
    values_fill = 0  # Llenar NAs con 0
  ) %>%
  arrange(Fleet, yLat5ctoid, xLon5ctoid)

# Ver las primeras filas de la tabla
print("Primeras filas de la tabla de proporciones por flota y grid:")
head(Species_proportion_table_grid)


# Versión con porcentajes
Species_proportion_pct_grid <- Fleets_catch_composition %>%
  select(Fleet, yLat5ctoid, xLon5ctoid, SpeciesCode, proportion) %>%
  mutate(proportion_pct = round(proportion * 100, 2)) %>%
  select(-proportion) %>%
  pivot_wider(
    names_from = SpeciesCode,
    values_from = proportion_pct,
    values_fill = 0
  ) %>%
  arrange(Fleet, yLat5ctoid, xLon5ctoid)

# Guardar tabla con porcentajes
write.csv(Species_proportion_pct_grid, "species_proportion_fleet_grid.csv", row.names = FALSE)


##ahora se filtra para eliminar las que tienen 100% de BUM en las dos flotas de PS 
##con puntos que sabemos que están mal
##
# Buscar las flotas específicas con 100% de BUM, como vector
target_fleets <- c("PS_EU.ESP-ES-ETRO", "PS_EU.FRA-FR-ETRO")

# Filtrar los puntos con 100% de BUM para estas flotas
bum_100_points <- Species_proportion_pct_grid %>%
  filter(Fleet %in% target_fleets & BUM == 100) %>%
  select(Fleet, yLat5ctoid, xLon5ctoid, BUM)

# Mostrar los puntos encontrados
print("Puntos con 100% de BUM encontrados:")
print(bum_100_points)
cat("Número de puntos con 100% de BUM:", nrow(bum_100_points), "\n")

# Si hay puntos con 100% de BUM, eliminarlos de la base de datos inicial
if(nrow(bum_100_points) > 0) {
  
  # Crear una copia de la base de datos original para no modificar la original
  CATDIS_clean <- CATDIS
  
  # Contar registros antes de la eliminación
  records_before <- nrow(CATDIS_clean)
  cat("Registros antes de eliminar:", records_before, "\n")
  
  # Eliminar los puntos identificados
  for(i in 1:nrow(bum_100_points)) {
    current_point <- bum_100_points[i, ]
    CATDIS_clean <- CATDIS_clean %>%
      filter(!(Fleet == current_point$Fleet & 
                 yLat5ctoid == current_point$yLat5ctoid & 
                 xLon5ctoid == current_point$xLon5ctoid))
  }}
  
  # Contar registros después de la eliminación
  records_after <- nrow(CATDIS_clean)
  cat("Registros después de eliminar:", records_after, "\n")
  cat("Registros eliminados:", records_before - records_after, "\n")
  
  # Guardar la base de datos limpia
  write.csv2(CATDIS_clean, "CATDIS_clean.csv", row.names = FALSE)
  cat("Base de datos limpia guardada como: CATDIS_clean_no_BUM100_2014-2023.csv\n")
  
  # También podemos eliminar estos puntos de la tabla de proporciones
  Species_proportion_pct_grid_clean <- Species_proportion_pct_grid %>%
    anti_join(bum_100_points, by = c("Fleet", "yLat5ctoid", "xLon5ctoid"))
  
  # Guardar la tabla de proporciones limpia
  write.csv(Species_proportion_pct_grid_clean, "species_proportion_clean.csv", row.names = FALSE)
  cat("Tabla de proporciones limpia guardada\n")
  
  # Verificar que se eliminaron correctamente
  bum_100_points_after <- Species_proportion_pct_grid_clean %>%
    filter(Fleet %in% target_fleets & BUM == 100)
  
  cat("Puntos con 100% de BUM después de la limpieza:", nrow(bum_100_points_after), "\n")
  
