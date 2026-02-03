##REASIGNAR PUNTOS QUE CAEN EN TIERRA Y QUE NO SE LES ASIGNÓ ECORREGION EN EL PASO
#ANTERIOR. Se utiliza el shapefile y CATDIS+SBT+ECOREGION

#primero el directorio
setwd("C:/...")

# --------------------------------------
# 1. Cargar paquetes y datos
# --------------------------------------
library(sf)       
library(dplyr)    
library(ggplot2)
library(nngeo)
library(rnaturalearth)

# Cargar datos de capturas (puntos)
catdis <- read.csv2("CATDIS_SBT_con_ecoregion.csv") 
# Duplicar las columnas yLat5ctoid y xLon5ctoid (añadir el nombre de latitud y longitud)
catdis$Lat3 <- catdis$Lat2
catdis$Lon3 <- catdis$Lon2

# Cargar shapefile de ecorregiones
ecoregions <- st_read("ECOREGIONS_SP_DP001.shp")  

# Verificar sistemas de referencia (CRS)
st_crs(ecoregions)  # Debe ser WGS84 (EPSG:4326) 

# --------------------------------------
# 2. Convertir datos a formato sf
# --------------------------------------
# Convertir puntos a objeto espacial sf
catdis_sf <- st_as_sf(catdis, 
                      coords = c("Lon3", "Lat3"), 
                      crs = st_crs(ecoregions))  # Mismo CRS que el shapefile

# --------------------------------------
# 3. Identificar puntos con NA en ECOREGION (fuera de ecorregiones)
# --------------------------------------
puntos_fuera <- catdis_sf[is.na(catdis_sf$ECOREGION), ]

if (nrow(puntos_fuera) > 0) {
  ecoregion_cercana <- st_nn(puntos_fuera, ecoregions, k = 1, progress = TRUE)  # k=1 para la más cercana
  catdis_sf[is.na(catdis_sf$ECOREGION), "ECOREGION"] <- ecoregions$ECOREGION[unlist(ecoregion_cercana)]
}

# --------------------------------------
# 5. Guardar y visualizar resultados
# --------------------------------------
# Guardar como CSV (sin geometría)
write.csv2(st_drop_geometry(catdis_sf), "CATDIS_SBT_ECOREGION_reasignada.csv", row.names = FALSE)

catdis <- read.csv2("CATDIS_SBT_ECOREGION_reasignada.csv")

# Visualizar

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
  geom_sf(data = ecoregions, fill = "lightgray", color = "gray") +
  geom_sf(data = catdis_sf, aes(color = ECOREGION), size = 1) +
  labs(title = "Puntos asignados a ecorregiones (NA reemplazados)") +
  theme_minimal()
