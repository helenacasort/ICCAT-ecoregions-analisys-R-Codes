#ESTE SCRIPT CALCULA LOS INDICADORES QUE DEFINEN COREFLEETS para la ecoregion Tropical
#utiliza la base de datos catdis, el resultado es un excel con cada 
#indicador que despues hay que procesar

setwd("C:/")

library(readr)
ICCAT <- read_csv2("CATDIS_clean.csv")
View(ICCAT)
ICCAT$X.1 <- NULL

names(ICCAT)
#renames
names(ICCAT)[names(ICCAT) == "Fleet"] <- "Gear_Fleet"
names(ICCAT)[names(ICCAT) == "xLon5ctoid"] <- "Lon3"
names(ICCAT)[names(ICCAT) == "yLat5ctoid"] <- "Lat3"

unique(ICCAT$Gear_Fleet) #392 fleets in the entire ICCAT convention area

#------------------------------------
#Identify fleets with catches in Tropical Atlantic Ecoregion, operating 2014 onwards
summary(as.factor(ICCAT$ECOREGION))

library(dplyr)
ICCAT_TAE_2014<-ICCAT %>% filter(ECOREGION=="Tropical Atlantic Ecoregion") %>% filter(YearC>=2014)
head(ICCAT_TAE_2014)

unique(ICCAT_TAE_2014$Gear_Fleet) # 82 

TAE_fleets<-unique(ICCAT_TAE_2014$Gear_Fleet) 


setwd("C:/")
write.csv(TAE_fleets,"TAE_Fleet_List.csv")

#------------------------------------
#Calculate catches of TAE fleets WITHIN TAE REGION

library(dplyr)

ICCAT_TAE_2014_total_annual_catch <- ICCAT_TAE_2014 %>% group_by(Gear_Fleet,YearC) %>% summarize(TotalCatch = sum(Catch_t))
head(ICCAT_TAE_2014_total_annual_catch)
unique(ICCAT_TAE_2014_total_annual_catch$Gear_Fleet) 

ICCAT_TAE_2014_total_catch <- ICCAT_TAE_2014_total_annual_catch %>% 
  group_by(Gear_Fleet) %>% summarize(Total_average_Catch_within_TAE = mean(TotalCatch), 
                TotalCatch_within_TAE = sum(TotalCatch)) 

head(as.data.frame(ICCAT_TAE_2014_total_catch))



#------------------------------------
#Calculate catches of TAE fleets WITHIN ICCAT AREA 

ICCAT_2014<-ICCAT %>% filter(YearC>=2014)

ICCAT_2014_TAE_fleets<-ICCAT_2014 %>% filter(Gear_Fleet %in% TAE_fleets)

unique(ICCAT_2014_TAE_fleets$Gear_Fleet)

# Calcula la captura total anual por flota en toda la ICCAT
ICCAT_2014_TAE_fleets_total_annual_catch <- ICCAT_2014_TAE_fleets %>% 
  group_by(Gear_Fleet,YearC) %>% summarize(TotalCatch = sum(Catch_t))
head(ICCAT_2014_TAE_fleets_total_annual_catch)
unique(ICCAT_2014_TAE_fleets_total_annual_catch$Gear_Fleet) #86

# Calcula las métricas promedio y total para cada flota en toda la ICCAT
ICCAT_2014_TAE_fleets_total_catch_ICCAT <- ICCAT_2014_TAE_fleets_total_annual_catch %>% 
  group_by(Gear_Fleet) %>% summarize(Total_average_Catch_within_ICCAT = mean(TotalCatch), 
                                     TotalCatch_within_ICCAT = sum(TotalCatch)) 

unique(ICCAT_2014_TAE_fleets_total_catch_ICCAT$Gear_Fleet)

#----merge both files based on fleet name (CAPTURAS DENTRO DE TAE VS. CAPTURAS EN TODA LA ICCAT)--------------------------------

TAE_fleets_catches<-merge(ICCAT_2014_TAE_fleets_total_catch_ICCAT,
                          ICCAT_TAE_2014_total_catch,by.x="Gear_Fleet",by.y="Gear_Fleet")

head(TAE_fleets_catches)

#Let's round up some columns (redondeo para entender mejor a 3 decimales)
TAE_fleets_catches$Total_average_Catch_within_ICCAT <- round(TAE_fleets_catches$Total_average_Catch_within_ICCAT, 3)
TAE_fleets_catches$Total_average_Catch_within_TAE <- round(TAE_fleets_catches$Total_average_Catch_within_TAE, 3)


#----calculate % of catches within TAE relative to entire ICCAT area-----relacion capturas ECOREG respecto tot-------------
TAE_fleets_catches$Per_total_catch_within_TAE =
  round(TAE_fleets_catches$TotalCatch_within_TAE/TAE_fleets_catches$
          TotalCatch_within_ICCAT*100,3)

head(as.data.frame(TAE_fleets_catches))


#----calculate TOTAL_TAE_CATCHES---LA CAPTURA TOTAL ACUMULADA POR TODAS LAS FLOTAS EN TAE------------------
TAE_fleets_catches2 <- TAE_fleets_catches  %>% 
  mutate(Total_catch_across_fleets_within_TAE =sum(TotalCatch_within_TAE))

head(as.data.frame(TAE_fleets_catches2))

#----calculate % of catches of TAE fleets relative to TOTAL_TAE_CATCHES------------------
TAE_fleets_catches2$Per_total_catch_within_TAE_relative_to_TOTAL_TAE_catch <-round(TAE_fleets_catches2$  
  TotalCatch_within_TAE/TAE_fleets_catches2$Total_catch_across_fleets_within_TAE*100,4)

head(as.data.frame(TAE_fleets_catches2))

unique(TAE_fleets_catches2$Gear_Fleet) #siguen saliendo las mismas 142 (86)

# ---- Calcular capturas totales por flota en todo ICCAT ----
total_catch_by_fleet_ICCAT <- ICCAT %>%
  filter(YearC >= 2014) %>% # <--- ¡Nuevo filtro aquí!
  group_by(Gear_Fleet) %>%  # Usando la misma columna que en tu análisis TAE
  summarise(TotalCatch_ICCAT = sum(Catch_t, na.rm = TRUE))  # Ajusta "Catch" al nombre de tu columna de capturas

# Verificar los resultados
head(total_catch_by_fleet_ICCAT)

# ---- Unir con tu dataset de TAE ----
TAE_fleets_catches2 <- TAE_fleets_catches2 %>%
  left_join(total_catch_by_fleet_ICCAT, by = "Gear_Fleet")
# Verificar que se unió correctamente
head(as.data.frame(TAE_fleets_catches2))


#### Mean catch top species per fleet inside de SC####
#### IDENTIFICAR LA ESPECIE PRINCIPAL CAPTURADA POR CADA FLOTA DENTRO DE TAE
TA_fleets_tca_sp <- ICCAT_TAE_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC)  %>%summarize(tCatch=sum(Catch_t)) 

# Para cada flota:
# 1. Calcula la captura promedio por especie.
# 2. Selecciona la fila (especie) con el promedio más alto para esa flota.
TA_fleets_meanTC <- TA_fleets_tca_sp %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(meanCatch_sp_TAE =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = meanCatch_sp_TAE)


#----merge both files based on fleet name---especie principal a la base principal----------------------

TAE_fleets_catches2 <- TAE_fleets_catches2 %>%
  left_join(TA_fleets_meanTC, by = "Gear_Fleet")

head(TAE_fleets_catches2)

#Let's fix some column names
#TAE_fleets_catches2 <- TAE_fleets_catches2 %>% rename(meanCatch_SC_sp = meanCatch_sp)
#cambio de nombre
names(TAE_fleets_catches2)[names(TAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_TAE"


##### Mean Catch top species outside TA ####
## Repite el proceso anterior pero ahora filtra FUERA de la ecorregión

ICCAT_fleets_tca_sp <- ICCAT_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC,ECOREGION)  %>%summarize(tCatch=sum(Catch_t)) 

ICCAT_fleets_tca_sp_noTA <- ICCAT_fleets_tca_sp %>% filter(ECOREGION!="Tropical Atlantic Ecoregion")

#Encuentra la especie top para cada flota fuera
ICCAT_fleets_meanTC_sp_noTA <- ICCAT_fleets_tca_sp_noTA %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(meanCatch_sp_outside_TAE =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = meanCatch_sp_outside_TAE)


# Merge the result back to the TA_df dataframe
TAE_fleets_catches2 <- TAE_fleets_catches2 %>%
  left_join(ICCAT_fleets_meanTC_sp_noTA, by = "Gear_Fleet")

head(TAE_fleets_catches2)

names(TAE_fleets_catches2)[names(TAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_outside_TAE"

head(TAE_fleets_catches2)

##### Mean Catch top species in ICCAT #### especie top cada flota en todo el area ICCAT

ICCAT_fleets_tca_sp <- ICCAT_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC,ECOREGION)  %>%summarize(tCatch=sum(Catch_t)) 

ICCAT_fleets_meanTC_sp_ICCAT <- ICCAT_fleets_tca_sp %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(MeanCatch_sp_ALL =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = MeanCatch_sp_ALL)

# Merge the result back to the SC_df dataframe
TAE_fleets_catches2 <- TAE_fleets_catches2 %>%
  left_join(ICCAT_fleets_meanTC_sp_ICCAT, by = "Gear_Fleet")

head(TAE_fleets_catches2)

names(TAE_fleets_catches2)[names(TAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_ALL"

head(TAE_fleets_catches2)

########################################

#AREA - PIXEL ANALYSIS 
########################################
# number of pixels with catch in ICCAT AREA
detach("package:plyr", unload = TRUE)#if fails, restart R session
library(dplyr)
ICCAT_2014<-ICCAT %>% filter(YearC>=2014)

ICCAT_2014_TAE_fleets<-ICCAT_2014 %>% filter(Gear_Fleet %in% TAE_fleets)

unique(ICCAT_2014_TAE_fleets$Gear_Fleet)

# Agrupa los datos de todas las flotas TAE en la ICCAT (desde 2014) por Flota, Longitud y Latitud.
# Suma las capturas en cada celda (pixel). Esto nos dice en qué celdas operó cada flota.
ICCAT_2014_TAE_fleets_total_catch_lonlat <- ICCAT_2014_TAE_fleets %>% 
  group_by(Gear_Fleet,Lon3,Lat3) %>% summarize(TotalCatch = sum(Catch_t,na.rm=T))

head(ICCAT_2014_TAE_fleets_total_catch_lonlat)

detach("package:dplyr", unload = TRUE)
library(plyr)
# Para cada flota, cuenta el número total de celdas únicas (pixeles) en las que reportó captura en toda la ICCAT
Number_total_pixels_ICCAT<-ddply(ICCAT_2014_TAE_fleets_total_catch_lonlat, 
                                 .(Gear_Fleet),summarize, 
                                 Number_total_pixels_fleet_in_ICCAT = length(Gear_Fleet))

head(Number_total_pixels_ICCAT) #en que pixeles (nº) esta presente cada flota

########################################
########################################
# number of pixels with catch within TAE
detach("package:plyr", unload = TRUE)
library(dplyr)
ICCAT_2014_withinTAE<-ICCAT %>% filter(YearC>=2014) %>% filter(ECOREGION=="Tropical Atlantic Ecoregion")

unique(ICCAT_2014_withinTAE$Gear_Fleet) #siguen siendo 146 (86)

# Agrupa los datos que solo están dentro de TAE por Flota, Longitud y Latitud
ICCAT_2014_withinTAE_total_catch_lonlat <- ICCAT_2014_withinTAE %>% 
  group_by(Gear_Fleet,Lon3,Lat3) %>% summarize(TotalCatch = sum(Catch_t,na.rm=T))

head(ICCAT_2014_withinTAE_total_catch_lonlat)

detach("package:dplyr", unload = TRUE)
library(plyr)

# Para cada flota, cuenta el número de celdas únicas en las que reportó captura dentro de TAE.
Number_total_pixels_TAE<-ddply(ICCAT_2014_withinTAE_total_catch_lonlat, 
                               .(Gear_Fleet),summarize, 
                               Number_total_pixels_fleet_inTAE_relative_to_ICCAT = length(Gear_Fleet))

head(Number_total_pixels_TAE)

#calculate percentage of pixel fished within TAE (relative to fished area in ICCAT)

Number_total_pixels<-merge(Number_total_pixels_TAE,Number_total_pixels_ICCAT,by.x="Gear_Fleet",by.y="Gear_Fleet")

head(Number_total_pixels)


#calculate percentage of pixels within TAE relative to all ICCAT area,

Number_total_pixels$
  per_pixels_withinTAE_relative_to_ICCATfished_area<-round(Number_total_pixels$
                                                             Number_total_pixels_fleet_inTAE_relative_to_ICCAT/Number_total_pixels$
                                                             Number_total_pixels_fleet_in_ICCAT*100,2)

head(Number_total_pixels)


########################################
########################################
# number of pixels per ecoregion
#remove plyr
detach("package:plyr", unload = TRUE)

library(dplyr)
Ecoregion_pixel <- ICCAT %>% group_by(ECOREGION,Lon3,Lat3) %>% 
  summarize(TotalCatch = sum(Catch_t,na.rm=T))

head(Ecoregion_pixel)

unique(as.factor(Ecoregion_pixel$ECOREGION))

Ecoregion_pixel$ECOREGION<-as.factor(Ecoregion_pixel$ECOREGION)

library(plyr)

Ecoregion_pixel2<-ddply(Ecoregion_pixel, .(ECOREGION),summarize, Number_total_pixels_ecoregion = length(ECOREGION))

Ecoregion_pixel2 ##TAE=79 PIXELS

########################################
########################################
# number of pixels per ecoregion
head(Number_total_pixels)

#HERE SUPER IMPORTANT TO CHANGE THE NUMBER ACCORDING TO THE PIXELS PER ECOREGION!!! TAE=79
Number_total_pixels$per_pixels_in_TAE_relative_to_TAE_AREA<-round(Number_total_pixels$Number_total_pixels_fleet_inTAE_relative_to_ICCAT/79*100,2)

head(Number_total_pixels)

###########################
#MERGE catch-based analysis and pixel-based analysis

TAE_analysis<-merge(TAE_fleets_catches2,Number_total_pixels,by.x="Gear_Fleet",by.y="Gear_Fleet")

names(TAE_analysis)

unique(TAE_analysis$Gear_Fleet) #sigue siendo 146 (118)

###################################
# MODIFICACIÓN: ANÁLISIS TEMPORAL USANDO TASK1_fleetmerge.csv, tak1 es más completa
###################################
TASK1 <- read_csv2("TASK1_clean.csv")
TASK1$...1 <- NULL
View(TASK1)

# Usar la lista de flotas TAE que ya identificamos del archivo original
# TAE_fleets contiene las flotas que operan en North Atlantic Ecoregion
# Filtrar TASK1 para incluir solo esas flotas desde 2014
TASK1_TAE_2014 <- TASK1 %>% 
  filter(Gear_Fleet_New %in% TAE_fleets) %>% 
  filter(YearC >= 2014)

head(TASK1_TAE_2014)

detach(plyr)
library(dplyr)

# Calcular captura anual total por flota usando TASK1 (solo flotas TAE)
TASK1_TAE_2014_total_annual_catch <- TASK1_TAE_2014 %>% 
  ungroup() %>%
  group_by(Gear_Fleet_New, YearC) %>% 
  summarize(TotalCatch = sum(Qty_t))


head(TASK1_TAE_2014_total_annual_catch)

# Calcular número de años con captura reportada desde 2014
detach("package:plyr", unload = TRUE)
library(dplyr)

Number_YearCs_with_catch_reported_since_2014 <- TASK1_TAE_2014_total_annual_catch %>% 
  group_by(Gear_Fleet_New) %>% 
  summarize(Number_YearCs_with_catch_reported_since_2014 = n())

head(Number_YearCs_with_catch_reported_since_2014)

# Calcular número de años con captura reportada desde 2019, ultimos 4 años
TASK1_TAE_2019_total_annual_catch <- TASK1_TAE_2014_total_annual_catch %>% 
  filter(YearC >= 2019) #cambio de 2016 a 2019

Number_YearCs_with_catch_reported_since_2019 <- TASK1_TAE_2019_total_annual_catch %>% 
  group_by(Gear_Fleet_New) %>% 
  summarize(Number_YearCs_with_catch_reported_since_2019 = n())

head(Number_YearCs_with_catch_reported_since_2019)

# Combinar los resultados temporales
YearCs_reporting <- merge(Number_YearCs_with_catch_reported_since_2014, 
                          Number_YearCs_with_catch_reported_since_2019, 
                          by = "Gear_Fleet_New", all = TRUE)
#rename
YearCs_reporting <- YearCs_reporting %>% rename(Gear_Fleet = Gear_Fleet_New)
head(YearCs_reporting)
unique(YearCs_reporting$Gear_Fleet) #112

#merge

YearCs_reporting<-merge(Number_YearCs_with_catch_reported_since_2014,Number_YearCs_with_catch_reported_since_2019,by.x="Gear_Fleet_New",by.y="Gear_Fleet_New",all=TRUE)
head(YearCs_reporting)
unique(YearCs_reporting$Gear_Fleet) #118
YearCs_reporting <- YearCs_reporting %>% rename(Gear_Fleet = Gear_Fleet_New)
#### merge with TAE_analysis

TAE_analysis2<-merge(TAE_analysis,YearCs_reporting,by.x="Gear_Fleet",by.y="Gear_Fleet",all=TRUE)

head(TAE_analysis2)

unique(TAE_analysis2$Gear_Fleet) #siguen saliendo 118

library(writexl)

setwd("C:/")
write_xlsx(TAE_analysis2, "TAE_analysis.xlsx")

write.csv(TAE_analysis2,"TAE_analysis.csv")
