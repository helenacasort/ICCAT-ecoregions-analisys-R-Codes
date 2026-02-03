##ESTE SRIPT REALIZA UN MERGE ENTRE SHAPEFILE QUE CONTIENE LAS ECOREGIONES Y 
#LA DB CATDIS DE ICCAT, SEGUN LAS COORDENADAS EN LAS QUE CAE EL PUNTO SE ASIGNA
#LA ECOREGION EN UNA NUEVA COLUMNA LLAMADA ECOREGION

#PARA LOGRAR UN TRATAMIENTO EFICAZ DE LOS DATOS SE CONSIDERA LA GEOMETRIA DE LA
#TIERRA COMO PLANA

#merge shapefile y db pero haciendo las geometrias planas
library(dplyr)
library(sf)
sf_use_s2(FALSE) # <--- Esta es la clave
library(dplyr)

#primero el directorio
setwd("C:...")

df <- read.csv("CATDIS_SBT_complete.csv") #CATDIS + CCSBT DB
df$...1 <- NULL
# Duplicar las columnas yLat5ctoid y xLon5ctoid
df$Lat2 <- df$yLat5ctoid
df$Lon2 <- df$xLon5ctoid

sf_points <- st_as_sf(df, coords = c("xLon5ctoid", "yLat5ctoid"), crs = 4326)

ecoregions_sf <- st_read("ECOREGIONS_SP_DP001.shp")

# Ejemplo de transformación de CRS si es necesario
if (st_crs(sf_points) != st_crs(ecoregions_sf)) {
 sf_points <- st_transform(sf_points, st_crs(ecoregions_sf))}

points_with_ecoregion <- st_join(sf_points, 
                                 ecoregions_sf["ECOREGION"], join = st_intersects)

points_to_save <- as.data.frame(points_with_ecoregion)
write.csv2(points_to_save, "C:/CATDIS_SBT_con_ecoregion.csv", 
          row.names = FALSE)
