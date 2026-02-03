##este script hace una nueva base de datos agrupando las flotas 
##seleccionadas en el excel, para ello se hace un rename de las flotas elegidas
##en la base total de iccat, despues de esa base se harán los analisis de corefleets otra vez

setwd("C:/")

library(readr)
library(dplyr)
library(sf)
library(tidyr)

ICCAT <- read_csv2("CATDIS_SBT_ECOREGION_reasignada.csv")
View(ICCAT)
ICCAT$...1 <- NULL
unique(ICCAT$Gear_Fleet) #574 fleets in the entire ICCAT convention area

#para poner la columna de GearFleet con la barra baja y ademas asignar other 
ICCAT <- ICCAT %>% 
  unite(GearGrp, FleetCode, col = "Gear_Fleet", sep = "_", remove = FALSE) %>%
  mutate(Gear_Fleet = ifelse(GearGrp == "oth", "Other", Gear_Fleet))
unique(ICCAT$Gear_Fleet) #pasa de 574 a 465, porque he quitado las Others


#a continuacion aparecen los cambios de nombre en una columna nueva
library(dplyr)

ICCAT <- ICCAT %>%
  mutate(Gear_Fleet_New = case_when(
    Gear_Fleet %in% c("BB_EU.ESP-ES-ETRO", "BB_EU.FRA-FR-ETRO") ~ "BB_EU-ETRO",
    Gear_Fleet %in% c("BB_GHA-GH-ETRO", "BB_GHA-GH-ETRO-A") ~ "BB_GHA-ETRO",
    Gear_Fleet %in% c("HL_SEN", "HL_SEN-SN-Art", "HL_SEN-SN-CVERT", 
                      "HL_SEN-SN-DAKAR", "HL_SEN-SN-Recr") ~ "HL_SEN",
    Gear_Fleet %in% c("LL_BLZ", "LL_BLZ-GH", "LL_BLZ-JP") ~ "LL_BLZ",
    Gear_Fleet %in% c("LL_CIV", "LL_CIV-KR") ~ "LL_CIV",
    Gear_Fleet %in% c("LL_NAM", "LL_NAM-CN", "LL_NAM-ES", "LL_NAM-JP", "LL_NAM-PT", "LL_NAM-SN",
                      "LL_NAM-TW", "LL_NAM-VC", "LL_NAM-VU") ~ "LL_NAM",
    Gear_Fleet %in% c("LL_SEN", "LL_SEN-SN-Art", "LL_SEN-SN-DAKAR") ~ "LL_SEN",
    Gear_Fleet %in% c("PS_BLZ", "PS_BLZ-BZ-ETRO", "PS_BLZ-GH") ~ "PS_BLZ",
    Gear_Fleet %in% c("PS_CPV", "PS_CPV-CV-ETRO") ~ "PS_CPV",
    Gear_Fleet %in% c("PS_GHA-GH-ETRO", "PS_GHA-GH-ETRO-A", "PS_GHA-GH-ETRO-P") ~ "PS_GHA",
    Gear_Fleet %in% c("PS_PAN-CI", "PS_PAN-PA-ETRO", "PS_PAN-PA-ETRO-FP") ~ "PS_PAN",
    Gear_Fleet %in% c("PS_SLV-PA-ETRO-FP", "PS_SLV-SV-ETRO", "PS_SLV-SV-ETRO-FP") ~ "PS_SLV",
    
    Gear_Fleet %in% c("BB_EU.ESP-ES-CANT_ALB", "BB_EU.ESP-ES-CANT_BFT") ~ "BB_EU.ESP",
    Gear_Fleet %in% c("BB_EU.FRA-FR", "BB_EU.FRA-FR-Recr") ~ "BB_EU.FRA",
    Gear_Fleet %in% c("PS_MAR", "PS_MAR-MA-ETRO") ~ "PS_MAR",
    Gear_Fleet %in% c("RR_EU.FRA-FR", "RR_EU.FRA-FR-Recr") ~ "RR_EU.FRA",
    Gear_Fleet %in% c("RR_EU.PRT-PT-MAINLND", "RR_EU.PRT-PT-MNLD-Recr") ~ "RR_EU.PT",
    Gear_Fleet %in% c("TR_EU.FRA-FR", "TR_EU.FRA-FR-Recr") ~ "TR_EU.FRA",
    
    Gear_Fleet %in% c("LL_ZAF", "LL_ZAF-JP") ~ "LL_ZAF",
    
    TRUE ~ Gear_Fleet))

unique(ICCAT$Gear_Fleet)#465 
unique(ICCAT$Gear_Fleet_New)#ahora tengo 436 de 465, despues 428

#cambio de nombre para que Gear_Fleet sea la de los nombres nuevos
ICCAT <- ICCAT %>% rename(Gear_Fleet_OLD = Gear_Fleet)
ICCAT <- ICCAT %>% rename(Gear_Fleet = Gear_Fleet_New)
write.csv2(ICCAT, "CATDIS_SBT_ECOR_fleetmerge.csv")
