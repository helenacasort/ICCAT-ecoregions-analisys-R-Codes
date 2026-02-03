#ESTE SCRIPT CALCULA LOS INDICADORES QUE DEFINEN COREFLEETS para la ecoregion Tropical
#utiliza la base de datos catdis, el resultado es un excel con cada 
#indicador que despues hay que procesar

setwd("C:/")

library(readr)
ICCAT <- read_csv2("CATDIS_clean.csv")
View(ICCAT)
ICCAT$X.1 <- NULL

names(ICCAT)

names(ICCAT)[names(ICCAT) == "Fleet"] <- "Gear_Fleet"
names(ICCAT)[names(ICCAT) == "xLon5ctoid"] <- "Lon3"
names(ICCAT)[names(ICCAT) == "yLat5ctoid"] <- "Lat3"

unique(ICCAT$Gear_Fleet) #428  fleets in the entire ICCAT convention area after fleetmerge

#------------------------------------
#Identify fleets with catches in North Atlantic Ecoregion, operating 2014 onwards
summary(as.factor(ICCAT$ECOREGION))

library(dplyr)
ICCAT_NAE_2014<-ICCAT %>% filter(ECOREGION=="North Atlantic Ecoregion") %>% filter(YearC>=2014)
head(ICCAT_NAE_2014)

unique(ICCAT_NAE_2014$Gear_Fleet) #102 fleets

NAE_fleets<-unique(ICCAT_NAE_2014$Gear_Fleet) 

setwd("C:/Users/helen/Desktop/TFM/COREFLEETS VALERIA/HELENA/NAE")
write.csv2(NAE_fleets,"NAE_Fleet_List_clean.csv")

#------------------------------------
#Calculate catches of NAE fleets WITHIN NAE REGION

library(dplyr)

ICCAT_NAE_2014_total_annual_catch <- ICCAT_NAE_2014 %>% group_by(Gear_Fleet,YearC) %>% summarize(TotalCatch = sum(Catch_t))
head(ICCAT_NAE_2014_total_annual_catch)
unique(ICCAT_NAE_2014_total_annual_catch$Gear_Fleet) 

# Toma el resultado anterior (capturas anuales por flota) y lo agrupa solo por Flota.
# 1. Total_average_Catch_within_NAE: La captura promedio por año (media de las capturas anuales).
# 2. TotalCatch_within_NAE: La captura total acumulada en todos los años.
ICCAT_NAE_2014_total_catch <- ICCAT_NAE_2014_total_annual_catch %>% 
  group_by(Gear_Fleet) %>% summarize(Total_average_Catch_within_NAE = mean(TotalCatch), 
                                     TotalCatch_within_NAE = sum(TotalCatch)) 

head(as.data.frame(ICCAT_NAE_2014_total_catch))

#------------------------------------
#Calculate catches of NAE fleets WITHIN ICCAT AREA 

ICCAT_2014<-ICCAT %>% filter(YearC>=2014)

ICCAT_2014_NAE_fleets<-ICCAT_2014 %>% filter(Gear_Fleet %in% NAE_fleets)

unique(ICCAT_2014_NAE_fleets$Gear_Fleet)

# Calcula la captura total anual por flota en toda la ICCAT
ICCAT_2014_NAE_fleets_total_annual_catch <- ICCAT_2014_NAE_fleets %>% 
  group_by(Gear_Fleet,YearC) %>% summarize(TotalCatch = sum(Catch_t))
head(ICCAT_2014_NAE_fleets_total_annual_catch)
unique(ICCAT_2014_NAE_fleets_total_annual_catch$Gear_Fleet) #112

# Calcula las métricas promedio y total para cada flota en toda la ICCAT
ICCAT_2014_NAE_fleets_total_catch_ICCAT <- ICCAT_2014_NAE_fleets_total_annual_catch %>% 
  group_by(Gear_Fleet) %>% summarize(Total_average_Catch_within_ICCAT = mean(TotalCatch), 
                                     TotalCatch_within_ICCAT = sum(TotalCatch)) 

unique(ICCAT_2014_NAE_fleets_total_catch_ICCAT$Gear_Fleet)

#----merge both files based on fleet name (CAPTURAS DENTRO DE NAE VS. CAPTURAS EN TODA LA ICCAT)--------------------------------

NAE_fleets_catches<-merge(ICCAT_2014_NAE_fleets_total_catch_ICCAT,
                          ICCAT_NAE_2014_total_catch,by.x="Gear_Fleet",by.y="Gear_Fleet")

head(NAE_fleets_catches)

#Let's round up some columns (redondeo para entender mejor a 3 decimales)
NAE_fleets_catches$Total_average_Catch_within_ICCAT <- round(NAE_fleets_catches$Total_average_Catch_within_ICCAT, 3)
NAE_fleets_catches$Total_average_Catch_within_NAE <- round(NAE_fleets_catches$Total_average_Catch_within_NAE, 3)


#----calculate % of catches within NAE relative to entire ICCAT area-----
NAE_fleets_catches$Per_total_catch_within_NAE =
  round(NAE_fleets_catches$TotalCatch_within_NAE/NAE_fleets_catches$
          TotalCatch_within_ICCAT*100,1)

head(as.data.frame(NAE_fleets_catches))


#----calculate TOTAL_NAE_CATCHES---LA CAPTURA TOTAL ACUMULADA POR TODAS LAS FLOTAS EN NAE---------
NAE_fleets_catches2 <- NAE_fleets_catches  %>% 
  mutate(Total_catch_across_fleets_within_NAE =sum(TotalCatch_within_NAE))

head(as.data.frame(NAE_fleets_catches2))

#----calculate % of catches of NAE fleets relative to TOTAL_NAE_CATCHES------------------------
NAE_fleets_catches2$Per_total_catch_within_NAE_relative_to_TOTAL_NAE_catch <-round(NAE_fleets_catches2$  
                                                                                     TotalCatch_within_NAE/NAE_fleets_catches2$Total_catch_across_fleets_within_NAE*100,4)

head(as.data.frame(NAE_fleets_catches2))

unique(NAE_fleets_catches2$Gear_Fleet) 

#### Mean catch top species per fleet inside de NAE#######
NA_fleets_tca_sp <- ICCAT_NAE_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC)  %>%summarize(tCatch=sum(Catch_t)) 

NA_fleets_meanTC <- NA_fleets_tca_sp %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(meanCatch_sp_NAE =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = meanCatch_sp_NAE)


#----merge both files based on fleet name---especie principal a la base principal----------------------

NAE_fleets_catches2 <- NAE_fleets_catches2 %>%
  left_join(NA_fleets_meanTC, by = "Gear_Fleet")

head(NAE_fleets_catches2)

#Let's fix some column names

names(NAE_fleets_catches2)[names(NAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_NAE"


##### Mean Catch top species outside NA ####

ICCAT_fleets_tca_sp <- ICCAT_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC,ECOREGION)  %>%summarize(tCatch=sum(Catch_t)) 

ICCAT_fleets_tca_sp_noNA <- ICCAT_fleets_tca_sp %>% filter(ECOREGION!="North Atlantic Ecoregion")

#Encuentra la especie top para cada flota fuera
ICCAT_fleets_meanTC_sp_noNA <- ICCAT_fleets_tca_sp_noNA %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(meanCatch_sp_outside_NAE =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = meanCatch_sp_outside_NAE)


# Merge the result back to the TA_df dataframe
NAE_fleets_catches2 <- NAE_fleets_catches2 %>%
  left_join(ICCAT_fleets_meanTC_sp_noNA, by = "Gear_Fleet")

head(NAE_fleets_catches2)

names(NAE_fleets_catches2)[names(NAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_outside_NAE"

head(NAE_fleets_catches2)

##### Mean Catch top species in ICCAT #### 

ICCAT_fleets_tca_sp <- ICCAT_2014 %>% 
  group_by(SpeciesCode,Gear_Fleet,YearC,ECOREGION)  %>%summarize(tCatch=sum(Catch_t)) 

ICCAT_fleets_meanTC_sp_ICCAT <- ICCAT_fleets_tca_sp %>% group_by(Gear_Fleet,SpeciesCode)%>% 
  summarize(MeanCatch_sp_ALL =round(mean(tCatch),2))%>%
  group_by(Gear_Fleet) %>%
  top_n(1, wt = MeanCatch_sp_ALL)

# Merge the result back to the SC_df dataframe
NAE_fleets_catches2 <- NAE_fleets_catches2 %>%
  left_join(ICCAT_fleets_meanTC_sp_ICCAT, by = "Gear_Fleet")

head(NAE_fleets_catches2)

names(NAE_fleets_catches2)[names(NAE_fleets_catches2) == "SpeciesCode"] <- "Top_sp_ALL"

head(NAE_fleets_catches2)


#AREA - PIXEL ANALYSIS
########################################
# number of pixels with catch in ICCAT AREA
#detach("package:plyr", unload = TRUE)
library(dplyr)
ICCAT_2014<-ICCAT %>% filter(YearC>=2014)

ICCAT_2014_NAE_fleets<-ICCAT_2014 %>% filter(Gear_Fleet %in% NAE_fleets)

unique(ICCAT_2014_NAE_fleets$Gear_Fleet)

ICCAT_2014_NAE_fleets_total_catch_lonlat <- ICCAT_2014_NAE_fleets %>% 
  group_by(Gear_Fleet,Lon3,Lat3) %>% summarize(TotalCatch = sum(Catch_t,na.rm=T))

head(ICCAT_2014_NAE_fleets_total_catch_lonlat)

detach("package:dplyr", unload = TRUE)
library(plyr)

Number_total_pixels_ICCAT<-ddply(ICCAT_2014_NAE_fleets_total_catch_lonlat, 
                                 .(Gear_Fleet),summarize, 
                                 Number_total_pixels_fleet_in_ICCAT = length(Gear_Fleet))

head(Number_total_pixels_ICCAT) #en que pixeles (nº) esta presente cada flota

########################################
########################################
# number of pixels with catch within NAE
detach("package:plyr", unload = TRUE)
library(dplyr)
ICCAT_2014_withinNAE<-ICCAT %>% filter(YearC>=2014) %>% filter(ECOREGION=="North Atlantic Ecoregion")

unique(ICCAT_2014_withinNAE$Gear_Fleet) 

# Agrupa los datos que solo están dentro de NAE por Flota, Longitud y Latitud
ICCAT_2014_withinNAE_total_catch_lonlat <- ICCAT_2014_withinNAE %>% 
  group_by(Gear_Fleet,Lon3,Lat3) %>% summarize(TotalCatch = sum(Catch_t,na.rm=T))

head(ICCAT_2014_withinNAE_total_catch_lonlat)

detach("package:dplyr", unload = TRUE)
library(plyr)

# Para cada flota, cuenta el número de celdas únicas en las que reportó captura dentro de NAE.
Number_total_pixels_NAE<-ddply(ICCAT_2014_withinNAE_total_catch_lonlat, 
                               .(Gear_Fleet),summarize, 
                               Number_total_pixels_fleet_inNAE_relative_to_ICCAT = length(Gear_Fleet))

head(Number_total_pixels_NAE)

#calculate percentage of pixel fished within NAE (relative to fished area in ICCAT)

Number_total_pixels<-merge(Number_total_pixels_NAE,Number_total_pixels_ICCAT,by.x="Gear_Fleet",by.y="Gear_Fleet")

head(Number_total_pixels)

#calculate percentage of pixels within NAE relative to all ICCAT area,

Number_total_pixels$
  per_pixels_withinNAE_relative_to_ICCATfished_area<-round(Number_total_pixels$
                                                             Number_total_pixels_fleet_inNAE_relative_to_ICCAT/Number_total_pixels$
                                                             Number_total_pixels_fleet_in_ICCAT*100,2)

head(Number_total_pixels)


########################################
########################################
# number of pixels per ecoregion, cuantos pixeles tiene  cada ecoregion
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

Ecoregion_pixel2 ##NAE=128 PIXELS

########################################
########################################
# number of pixels per ecoregion
head(Number_total_pixels)

#HERE SUPER IMPORTANT TO CHANGE THE NUMBER ACCORDING TO THE PIXELS PER ECOREGION!!! NAE=128
Number_total_pixels$per_pixels_in_NAE_relative_to_NAE_AREA<-round(Number_total_pixels$Number_total_pixels_fleet_inNAE_relative_to_ICCAT/128*100,2)

head(Number_total_pixels)

###########################
#MERGE catch-based analysis and pixel-based analysis

NAE_analysis<-merge(NAE_fleets_catches2,Number_total_pixels,by.x="Gear_Fleet",by.y="Gear_Fleet")

names(NAE_analysis)

unique(NAE_analysis$Gear_Fleet) #sigue siendo 112

###################################
# MODIFICACIÓN: ANÁLISIS TEMPORAL USANDO TASK1_fleetmerge.csv
###################################
# Leer el archivo TASK1_fleetmerge.csv para el análisis temporal
TASK1 <- read_csv2("TASK1_clean.csv")
TASK1$...1 <- NULL
View(TASK1)

# Usar la lista de flotas NAE que ya identificamos del archivo original
# NAE_fleets contiene las flotas que operan en North Atlantic Ecoregion
# Filtrar TASK1 para incluir solo esas flotas desde 2014
TASK1_NAE_2014 <- TASK1 %>% 
  filter(Gear_Fleet_New %in% NAE_fleets) %>% 
  filter(YearC >= 2014)

head(TASK1_NAE_2014)
library(dplyr)

# Calcular captura anual total por flota usando TASK1 (solo flotas NAE)
TASK1_NAE_2014_total_annual_catch <- TASK1_NAE_2014 %>% 
  ungroup() %>%
  group_by(Gear_Fleet_New, YearC) %>% 
  summarize(TotalCatch = sum(Qty_t))


head(TASK1_NAE_2014_total_annual_catch)

# Calcular número de años con captura reportada desde 2014
detach("package:plyr", unload = TRUE)
library(dplyr)

Number_YearCs_with_catch_reported_since_2014 <- TASK1_NAE_2014_total_annual_catch %>% 
  group_by(Gear_Fleet_New) %>% 
  summarize(Number_YearCs_with_catch_reported_since_2014 = n())

head(Number_YearCs_with_catch_reported_since_2014)

# Calcular número de años con captura reportada desde 2019, ultimos 4 años
TASK1_NAE_2019_total_annual_catch <- TASK1_NAE_2014_total_annual_catch %>% 
  filter(YearC >= 2019) #cambio de 2016 a 2019

Number_YearCs_with_catch_reported_since_2019 <- TASK1_NAE_2019_total_annual_catch %>% 
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

#### merge with NAE_analysis

NAE_analysis2 <- merge(NAE_analysis, YearCs_reporting, 
                       by.x = "Gear_Fleet", by.y = "Gear_Fleet", all = TRUE)

head(NAE_analysis2)

unique(NAE_analysis2$Gear_Fleet)


library(writexl)

setwd("C:/")
write_xlsx(NAE_analysis2, "NAE_analysis_CLEAN.xlsx")
write.csv(NAE_analysis2,"NAE_analysis_CLEAN.csv")
