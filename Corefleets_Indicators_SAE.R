#ESTE SCRIPT CALCULA LOS INDICADORES QUE DEFINEN COREFLEETS para la ecoregion South
#utiliza la base de datos catdis, el resultado es un excel con cada 
#indicador que despues hay que procesar
#para el analisis temporal usa los datos de TASK1 pero tambien los de SBT

setwd("C:/")

library(readr)
ICCAT <- read_csv2("CATDIS_clean.csv")
View(ICCAT)
ICCAT$X.1 <- NULL

names(ICCAT)
names(ICCAT)[names(ICCAT) == "Fleet"] <- "Gear_Fleet"
names(ICCAT)[names(ICCAT) == "xLon5ctoid"] <- "Lon3"
names(ICCAT)[names(ICCAT) == "yLat5ctoid"] <- "Lat3"


unique(ICCAT$Gear_Fleet) #391 fleets in the entire ICCAT convention area after fleetmerge

#------------------------------------
#Identify fleets with catches in South Atlantic Ecoregion, operating 2014 onwards
summary(as.factor(ICCAT$ECOREGION))

library(dplyr)
ICCAT_SAE_2014<-ICCAT %>% filter(ECOREGION=="South Atlantic Ecoregion") %>% filter(YearC>=2014)
head(ICCAT_SAE_2014)

unique(ICCAT_SAE_2014$Gear_Fleet) #38

SAE_fleets<-unique(ICCAT_SAE_2014$Gear_Fleet) 

setwd("C:/")
write.csv2(SAE_fleets,"SAEmerged_Fleet_List_clean.csv")
# Convertir el vector a dataframe
SAE_fleets_df <- data.frame(Fleet = SAE_fleets)
# Ahora sí puedes guardarlo como Excel
write_xlsx(SAE_fleets_df, "SAEmerged_Fleet_List.xlsx")
#------------------------------------
#Calculate catches of SAE fleets WITHIN SAE REGION

library(dplyr)

ICCAT_SAE_2014_total_annual_catch <- ICCAT_SAE_2014 %>% group_by(Gear_Fleet,YearC) %>% summarize(totalCatch = sum(Catch_t))

head(ICCAT_SAE_2014_total_annual_catch)
unique(ICCAT_SAE_2014_total_annual_catch$Gear_Fleet) #86

# Toma el resulSAdo anterior (capturas anuales por flota) y lo agrupa solo por flota.
# 1. total_average_Catch_within_SAE: La captura promedio por año (media de las capturas anuales).
# 2. totalCatch_within_SAE: La captura total acumulada en todos los años.
ICCAT_SAE_2014_total_catch <- ICCAT_SAE_2014_total_annual_catch %>% 
  group_by(Gear_Fleet) %>% summarize(total_average_Catch_within_SAE = mean(totalCatch), 
                                     totalCatch_within_SAE = sum(totalCatch)) 

head(as.data.frame(ICCAT_SAE_2014_total_catch))

#------------------------------------
#Calculate catches of SAE fleets WITHIN ICCAT AREA 

ICCAT_2014<-ICCAT %>% filter(YearC>=2014)

ICCAT_2014_SAE_fleets<-ICCAT_2014 %>% filter(Gear_Fleet %in% SAE_fleets)

unique(ICCAT_2014_SAE_fleets$Gear_Fleet)

# Calcula la captura total anual por flota en toda la ICCAT
ICCAT_2014_SAE_fleets_total_annual_catch <- ICCAT_2014_SAE_fleets %>% 
  group_by(Gear_Fleet,YearC) %>% summarize(totalCatch = sum(Catch_t))
head(ICCAT_2014_SAE_fleets_total_annual_catch)
unique(ICCAT_2014_SAE_fleets_total_annual_catch$Gear_Fleet) #86

# Calcula las métricas promedio y total para cada flota en toda la ICCAT
ICCAT_2014_SAE_fleets_total_catch_ICCAT <- ICCAT_2014_SAE_fleets_total_annual_catch %>% 
  group_by(Gear_Fleet) %>% summarize(total_average_Catch_within_ICCAT = mean(totalCatch), 
                                     totalCatch_within_ICCAT = sum(totalCatch)) 

unique(ICCAT_2014_SAE_fleets_total_catch_ICCAT$Gear_Fleet)

#----merge both files based on fleet name (CAPTURAS DENTRO DE SAE VS. CAPTURAS EN TODA LA ICCAT)--------------------------------

SAE_fleets_catches<-merge(ICCAT_2014_SAE_fleets_total_catch_ICCAT,
                          ICCAT_SAE_2014_total_catch,by.x="Gear_Fleet",by.y="Gear_Fleet")

head(SAE_fleets_catches)

#Let's round up some columns (redondeo para entender mejor a 3 decimales)
SAE_fleets_catches$total_average_Catch_within_ICCAT <- round(SAE_fleets_catches$total_average_Catch_within_ICCAT, 3)
SAE_fleets_catches$total_average_Catch_within_SAE <- round(SAE_fleets_catches$total_average_Catch_within_SAE, 3)


#----calculate % of catches within SAE relative to entire ICCAT area-----relacion capturas ECOREG respecto tot---------------------------
# Para cada flota: (Captura total en SAE / Captura total en ICCAT) * 100
# Esto muestra qué porcenSAje de la actividad total de la flota ocurre en SAE.
SAE_fleets_catches$Per_total_catch_within_SAE =
  round(SAE_fleets_catches$totalCatch_within_SAE/SAE_fleets_catches$
          totalCatch_within_ICCAT*100,1)

head(as.data.frame(SAE_fleets_catches))


#----calculate total_SAE_CATCHES---LA CAPTURA total ACUMULADA POR TODAS LAS flotas EN SAE-----------------------------
#Crea una nueva columna que, para cada flota, repite la suma total de capturas de TODAS las flotas en SAE
SAE_fleets_catches2 <- SAE_fleets_catches  %>% 
  mutate(total_catch_across_fleets_within_SAE =sum(totalCatch_within_SAE))

head(as.data.frame(SAE_fleets_catches2))

#----calculate % of catches of SAE fleets relative to TOTAL_SAE_CATCHES--------------------------------
#Para cada floTA: (Captura de la flota en SAE / Captura total de TODAS las flotas en SAE) * 100
# Esto muestra la imporSAncia relativa de cada flota para la ecorregión
SAE_fleets_catches2$Per_total_catch_within_SAE_relative_to_total_SAE_catch <-round(SAE_fleets_catches2$  
                                                                                     totalCatch_within_SAE/SAE_fleets_catches2$total_catch_across_fleets_within_SAE*100,4)

head(as.data.frame(SAE_fleets_catches2))

unique(SAE_fleets_catches2$Gear_Fleet)


#### Mean catch top species per fleet inside de SC####
SA_fleets_tca_sp <- ICCAT_SAE_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC)  %>%summarize(tCatch=sum(Catch_t)) 

# Para cada flota:
# 1. Calcula la captura promedio por especie.
# 2. Selecciona la fila (especie) con el promedio más alto para esa flota.
SA_fleets_meanTC <- SA_fleets_tca_sp %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(meanCatch_sp_SAE =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = meanCatch_sp_SAE)


#----merge both files based on fleet name--------------------

SAE_fleets_catches2 <- SAE_fleets_catches2 %>%
  left_join(SA_fleets_meanTC, by = "Gear_Fleet")

head(SAE_fleets_catches2)

#Let's fix some column names
names(SAE_fleets_catches2)[names(SAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_SAE"


##### Mean Catch top species outside SA ####
## Repite el proceso anterior pero ahora filtra FUERA de la ecorregión

ICCAT_fleets_tca_sp <- ICCAT_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC,ECOREGION)  %>%summarize(tCatch=sum(Catch_t)) 

ICCAT_fleets_tca_sp_noSA <- ICCAT_fleets_tca_sp %>% filter(ECOREGION!="South Atlantic Ecoregion")

#Encuentra la especie top para cada flota fuera
ICCAT_fleets_meanTC_sp_noSA <- ICCAT_fleets_tca_sp_noSA %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(meanCatch_sp_outside_SAE =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = meanCatch_sp_outside_SAE)


# Merge the result back to the SA_df dataframe
SAE_fleets_catches2 <- SAE_fleets_catches2 %>%
  left_join(ICCAT_fleets_meanTC_sp_noSA, by = "Gear_Fleet")

head(SAE_fleets_catches2)

names(SAE_fleets_catches2)[names(SAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_outside_SAE"

head(SAE_fleets_catches2)

##### Mean Catch top species in ICCAT #### especie top cada flota en todo el area ICCAT

ICCAT_fleets_tca_sp <- ICCAT_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC,ECOREGION)  %>%summarize(tCatch=sum(Catch_t)) 

ICCAT_fleets_meanTC_sp_ICCAT <- ICCAT_fleets_tca_sp %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(MeanCatch_sp_ALL =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = MeanCatch_sp_ALL)

# Merge the result back to the SAE df dataframe
SAE_fleets_catches2 <- SAE_fleets_catches2 %>%
  left_join(ICCAT_fleets_meanTC_sp_ICCAT, by = "Gear_Fleet")

head(SAE_fleets_catches2)

names(SAE_fleets_catches2)[names(SAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_ALL"

head(SAE_fleets_catches2)

########################################

#AREA - PIXEL ANALYSIS
########################################
# number of pixels with catch in ICCAT AREA
#detach("package:plyr", unload = TRUE)
library(dplyr)
ICCAT_2014<-ICCAT %>% filter(YearC>=2014)

ICCAT_2014_SAE_fleets<-ICCAT_2014 %>% filter(Gear_Fleet %in% SAE_fleets)

unique(ICCAT_2014_SAE_fleets$Gear_Fleet)

ICCAT_2014_SAE_fleets_total_catch_lonlat <- ICCAT_2014_SAE_fleets %>% 
  group_by(Gear_Fleet,Lon3,Lat3) %>% summarize(totalCatch = sum(Catch_t,na.rm=T))

head(ICCAT_2014_SAE_fleets_total_catch_lonlat)

detach("package:dplyr", unload = TRUE)
library(plyr)
# Para cada flota, cuenSA el número total de celdas únicas (pixeles) en las que reportó captura en toda la ICCAT
Number_total_pixels_ICCAT<-ddply(ICCAT_2014_SAE_fleets_total_catch_lonlat, 
                                 .(Gear_Fleet),summarize, 
                                 Number_total_pixels_fleet_in_ICCAT = length(Gear_Fleet))

head(Number_total_pixels_ICCAT) 

########################################
########################################
# number of pixels with catch within SAE
detach("package:plyr", unload = TRUE)
library(dplyr)
ICCAT_2014_withinSAE<-ICCAT %>% filter(YearC>=2014) %>% filter(ECOREGION=="South Atlantic Ecoregion")

unique(ICCAT_2014_withinSAE$Gear_Fleet) 

# Agrupa los datos que solo están dentro de SAE por flota, Longitud y Latitud
ICCAT_2014_withinSAE_total_catch_lonlat <- ICCAT_2014_withinSAE %>% 
  group_by(Gear_Fleet,Lon3,Lat3) %>% summarize(totalCatch = sum(Catch_t,na.rm=T))

head(ICCAT_2014_withinSAE_total_catch_lonlat)

detach("package:dplyr", unload = TRUE)
library(plyr)

# Para cada flota, cuenSA el número de celdas únicas en las que reportó captura dentro de SAE.
Number_total_pixels_SAE<-ddply(ICCAT_2014_withinSAE_total_catch_lonlat, 
                               .(Gear_Fleet),summarize, 
                               Number_total_pixels_fleet_inSAE_relative_to_ICCAT = length(Gear_Fleet))

head(Number_total_pixels_SAE)

#CALCULAR EL % DE PIXELES PESCADOS EN SAE vs. total DE PIXELES PESCADOS EN ICCAT
#calculate percenSAge of pixel fished within SAE (relative to fished area in ICCAT)
## merge

Number_total_pixels<-merge(Number_total_pixels_SAE,Number_total_pixels_ICCAT,by.x="Gear_Fleet",by.y="Gear_Fleet")

head(Number_total_pixels)


#calculate percentage of pixels within SAE relative to all ICCAT area,
#porcenSAje del área de operación de la flota que se concentra en SAE.

Number_total_pixels$
  per_pixels_withinSAE_relative_to_ICCATfished_area<-round(Number_total_pixels$
                                                             Number_total_pixels_fleet_inSAE_relative_to_ICCAT/Number_total_pixels$
                                                             Number_total_pixels_fleet_in_ICCAT*100,2)

head(Number_total_pixels)


########################################
########################################
# number of pixels per ecoregion, cuantos pixeles tiene  cada ecoregion
#remove plyr
detach("package:plyr", unload = TRUE)

library(dplyr)
Ecoregion_pixel <- ICCAT %>% group_by(ECOREGION,Lon3,Lat3) %>% 
  summarize(totalCatch = sum(Catch_t,na.rm=T))

head(Ecoregion_pixel)

unique(as.factor(Ecoregion_pixel$ECOREGION))

Ecoregion_pixel$ECOREGION<-as.factor(Ecoregion_pixel$ECOREGION)

library(plyr)

Ecoregion_pixel2<-ddply(Ecoregion_pixel, .(ECOREGION),summarize, Number_total_pixels_ecoregion = length(ECOREGION))

Ecoregion_pixel2 ##SAE=130 PIXELS

########################################
########################################
# number of pixels per ecoregion
head(Number_total_pixels)

#HERE SUPER IMPORSANT TO CHANGE THE NUMBER ACCORDING TO THE PIXELS PER ECOREGION!!! SAE=130
# Esto muestra qué porcenSAje del área total de SAE fue "cubierto" o "utilizado" por la flota.
Number_total_pixels$per_pixels_in_SAE_relative_to_SAE_AREA<-round(Number_total_pixels$Number_total_pixels_fleet_inSAE_relative_to_ICCAT/130*100,2)

head(Number_total_pixels)

###########################
#MERGE catch-based analysis and pixel-based analysis

SAE_analysis<-merge(SAE_fleets_catches2,Number_total_pixels,by.x="Gear_Fleet",by.y="Gear_Fleet")

names(SAE_analysis)

unique(SAE_analysis$Gear_Fleet) #sigue siendo 146 (86)

###################################
# MODIFICACIÓN: ANÁLISIS TEMPORAL COMBINANDO TASK1 E ICCAT por los datos de SBT
###################################
# Leer el archivo TASK1_fleetmerge.csv para el análisis temporal
setwd('C:/Users/helen/Desktop/TFM/COREFLEETS VALERIA/HELENA/SAE')
TASK1 <- read_csv2("TASK1_clean.csv")
TASK1$...1 <- NULL

View(TASK1)

# Filtrar TASK1 para flotas SAE desde 2014 (excluyendo BB_SBT y LL_SBT que no están)
TASK1_SAE_2014 <- TASK1 %>% 
  filter(Gear_Fleet_New %in% SAE_fleets) %>% 
  filter(YearC >= 2014)

head(TASK1_SAE_2014)
library(dplyr)

# Obtener datos de BB_SBT y LL_SBT desde ICCAT
ICCAT_BB_LL_2014 <- ICCAT %>% 
  filter(Gear_Fleet %in% c("BB_SBT", "LL_SBT")) %>% 
  filter(YearC >= 2014)

names(ICCAT_BB_LL_2014)[names(ICCAT_BB_LL_2014) == "Gear_Fleet"] <- "Gear_Fleet_New"
names(ICCAT_BB_LL_2014)[names(ICCAT_BB_LL_2014) == "Catch_t"] <- "Qty_t"

# COMBINAR: TASK1 (todas las flotas SAE excepto BB_SBT/LL_SBT) + ICCAT (solo BB_SBT/LL_SBT)
TASK1_combined <- bind_rows(TASK1_SAE_2014, ICCAT_BB_LL_2014)

library(dplyr)
# Calcular captura anual total con el dataset combinado
TASK1_combined_annual_catch <- TASK1_combined %>% 
  ungroup() %>%
  group_by(Gear_Fleet_New, YearC) %>% 
  summarize(TotalCatch = sum(Qty_t))

head(TASK1_combined_annual_catch)

# El resto del script sigue igual pero usando el dataset combinado
detach("package:plyr", unload = TRUE)

# Número de años con captura reportada desde 2014
Number_YearCs_with_catch_reported_since_2014 <- TASK1_combined_annual_catch %>% 
  group_by(Gear_Fleet_New) %>% 
  summarize(Number_YearCs_with_catch_reported_since_2014 = n())

# Número de años con captura reportada desde 2019
Number_YearCs_with_catch_reported_since_2019 <- TASK1_combined_annual_catch %>% 
  filter(YearC >= 2019) %>%
  group_by(Gear_Fleet_New) %>% 
  summarize(Number_YearCs_with_catch_reported_since_2019 = n())

# Combinar resultados temporales
YearCs_reporting <- merge(Number_YearCs_with_catch_reported_since_2014, 
                          Number_YearCs_with_catch_reported_since_2019, 
                          by = "Gear_Fleet_New", all = TRUE)

YearCs_reporting <- YearCs_reporting %>% rename(Gear_Fleet = Gear_Fleet_New)

head(YearCs_reporting)
unique(YearCs_reporting$Gear_Fleet)

#### merge with SAE_analysis
SAE_analysis2 <- merge(SAE_analysis, YearCs_reporting, 
                       by.x = "Gear_Fleet", by.y = "Gear_Fleet", all = TRUE)

head(SAE_analysis2)

unique(SAE_analysis2$Gear_Fleet)


library(writexl)

setwd("C:/")
write_xlsx(SAE_analysis2, "SAE_analysis_clean.xlsx")
write.csv(SAE_analysis2,"SAE_analysis_clean.csv")
