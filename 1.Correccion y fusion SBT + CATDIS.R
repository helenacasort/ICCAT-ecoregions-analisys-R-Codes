#1##CORRECCION DEL GRID DE SBT, DB extraida del .XLSX del website de CCSBT, 
library(readr)

setwd("C:/")

#Primero leer SBT
SBT_NA <- read_csv("SBT_NA.csv")

#2##DESPUÉS CORREGIR EL GRID DE SBT PARA QUE COINCIDA CON CATDIS ICCAT
df <- SBT_NA %>%
  mutate(Longitude = Longitude + 2.5) %>% mutate(Latitude = Latitude - 2.5)

# Filter the Atlantic ocean
df <- df %>% filter(Ocean =="Atlantic")

#anadir columna con FleetCode SBT
df <- df %>%  mutate(FleetCode = "SBT")

write.csv(df, "SBT_adapt_withNA.csv")

#3##UNIR SBT CORREGIDO Y CATDIS
#Leer catdis
CATDIS <- read_csv("cdis5023-all9sp.csv", col_types = cols(...1 = col_skip()))
CATDIS$Gear_Fleet <- paste(CATDIS$GearGrp, 
                          CATDIS$FleetCode, sep = "-")

#Cambiamos algunas columnas de nombre
names(CATDIS)[names(CATDIS)=="Catch_t"]<-"Tonnes"
names(CATDIS)[names(CATDIS)=="yLat5ctoid"]<-"Latitude"
names(CATDIS)[names(CATDIS)=="xLon5ctoid"]<-"Longitude"
names(CATDIS)[names(CATDIS)=="YearC"]<-"Year"


#UNIR CATDIS Y df
DATOS <- bind_rows(CATDIS, df)

#borrar columnas de SBT que sobran
DATOS$Ocean <- NULL
DATOS$Month <- NULL

#Cambiamos algunas columnas de nombre
names(DATOS)[names(DATOS)=="Tonnes"]<-"Catch_t"
names(DATOS)[names(DATOS)=="Latitude"]<-"yLat5ctoid"
names(DATOS)[names(DATOS)=="Longitude"]<-"xLon5ctoid"
names(DATOS)[names(DATOS)=="Year"]<-"YearC"

write.csv(DATOS, "CATDIS_SBT_complete.csv")

#4##PLOT PARA COMPROBAR LA CORRECCION
##PLOT SBT -  ICCAT

library(ggplot2)
library(sf)
library(rnaturalearth)

setwd("C:/")
CATDIS_SBT <- read.csv("CATDIS_SBT_complete.csv")  #tiene unida la info de CATDIS y SBT por campos
#duplicar Lon Lat
CATDIS_SBT$Lat2 <- CATDIS_SBT$yLat5ctoid
CATDIS_SBT$Lon2 <- CATDIS_SBT$xLon5ctoid


#Plot puntos en mapa
# 1. Obtener mapa mundial
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
  # Capa de puntos - azul para SBT, negro para otros
  geom_point(data = CATDIS_SBT, 
             aes(x = Lon2, y = Lat2, 
                 color = ifelse(SpeciesCode == "SBT", "SBT", "Otras especies")),
             size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("SBT" = "blue", "Otras especies" = "black"),
                     name = "Especie") +
  
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr") +
  
  labs(title = "Distribución por especie",
       subtitle = "Azul: SBT | Negro: Otras especies",
       x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "bottom")


