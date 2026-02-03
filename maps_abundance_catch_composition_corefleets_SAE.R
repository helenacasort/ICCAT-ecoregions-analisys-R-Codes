library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scatterpie)
library(patchwork)
library(rnaturalearth)

# --- 1. RUTAS Y CARGA DE DATOS ---
# Ajusta estas rutas a tu ordenador
path_base <- "C:/Users/helen/Desktop/TFM/COREFLEETS VALERIA/HELENA/SAE/clean"
path_salida <- file.path(path_base, "Mapas_Flotas_SAE")

if (!dir.exists(path_salida)) dir.create(path_salida)

setwd(path_base)

# Cargar archivos de flotas y CATDIS
catdis     <- read.csv2("CATDIS_clean.csv")
reg_fleets <- read.csv2("SAE_regional_fleets.csv")
acr_fleets <- read.csv2("SAE_acrossfleets.csv")

# Mapas base (usando tus rutas del script de Gloria)
Ecoregions <- sf::st_read("C:/Users/helen/Desktop/TFM/PLOTS GLORIA/ECOREGIONS_SP_DP001.shp")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
bbox_iccat <- sf::st_bbox(c(xmin= -105, ymin=-60, xmax=55, ymax=70), crs = st_crs(world))
world_iccat <- sf::st_crop(world, bbox_iccat)

# --- 2. PREPARACIÓN DE LISTA DE FLOTAS ---
# Unimos los nombres de todas las flotas que queremos mapear
todas_las_flotas <- unique(c(reg_fleets$Fleet, acr_fleets$Fleet, "LL_SBT", "BB_SBT"))

# --- 3. PROCESAMIENTO ESPACIAL ---
# Filtramos CATDIS solo para estas flotas y años recientes
catdis_SAE <- catdis %>%
  filter(Fleet %in% todas_las_flotas, YearC >= 2014) %>%
  mutate(Catch_t = as.numeric(Catch_t))

# A. Datos para burbujas (Media de totales anuales)
map_total <- catdis_SAE %>%
  group_by(yLat5ctoid, xLon5ctoid, Fleet, YearC) %>%
  summarise(total_anual = sum(Catch_t, na.rm = TRUE), .groups = "drop") %>%
  group_by(yLat5ctoid, xLon5ctoid, Fleet) %>%
  summarise(mean_Catch = mean(total_anual, na.rm = TRUE), .groups = "drop")

# B. Datos para tartas (Composición de especies)
map_pie <- catdis_SAE %>%
  group_by(yLat5ctoid, xLon5ctoid, Fleet, SpeciesCode) %>%
  summarise(mcatch = sum(Catch_t, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(freq = mcatch / sum(mcatch), radius = 1.8) %>% 
  select(-mcatch) %>%
  pivot_wider(names_from = SpeciesCode, values_from = freq, values_fill = 0)

# Colores de especies (tu paleta oficial)
sp_cols <- c(
  'SBT' = 'darkolivegreen1', 'BFT'= 'olivedrab', 'ALB' = 'olivedrab3',
  'SWO' = 'deepskyblue4', 'BUM'= 'deepskyblue3', 'WHM'= 'lightskyblue3', 'SAI'= 'lightcyan',
  'BET' = 'yellow', 'YFT' = 'orange', 'SKJ' = 'red', 
  'BSH' = 'orchid4', 'POR' = 'orchid', 'SMA' = 'orchid1', 'Other sharks' = 'pink',
  'Small Tuna' = 'snow3', 'Other Tuna and tuna like species' = 'chocolate', 'Other species' = 'chocolate4'
)

# --- 4. FUNCIÓN DE MAPEO POR FLOTA ---
mapear_flota <- function(nombre_flota) {
  
  df_puntos <- map_total %>% filter(Fleet == nombre_flota)
  df_pies   <- map_pie %>% filter(Fleet == nombre_flota)
  
  if(nrow(df_puntos) == 0) return(NULL)
  
  cols_presentes <- intersect(names(sp_cols), names(df_pies))
  
  # Mapa base estilizado
  base <- ggplot() +
    geom_sf(data = world_iccat, fill = "gray92", color = "gray70") +
    geom_sf(data = Ecoregions, fill = NA, color = "darkblue", linewidth = 0.4, alpha = 0.5) +
    coord_sf(xlim = bbox_iccat[c(1,3)], ylim = bbox_iccat[c(2,4)], expand = FALSE) +
    theme_minimal() + labs(x = NULL, y = NULL)
  
  # Plot 1: Abundancia
  p1 <- base +
    geom_point(data = df_puntos, aes(x = xLon5ctoid, y = yLat5ctoid, size = mean_Catch), 
               color = "firebrick", alpha = 0.6) +
    scale_size_continuous(name = "Avg Catch (t)", range = c(1, 10)) +
    ggtitle("Average Catch (2014-2023)")
  
  # Plot 2: Especies
  p2 <- base +
    geom_scatterpie(data = df_pies, aes(x = xLon5ctoid, y = yLat5ctoid, r = radius), 
                    cols = cols_presentes, color = NA, alpha = 0.8) +
    scale_fill_manual(values = sp_cols, name = "Species") +
    ggtitle("Species Composition")
  
  # Combinar
  final <- (p1 / p2) + 
    plot_annotation(title = paste("Fleet Mapping:", nombre_flota),
                    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))) +
    plot_layout(guides = "collect")
  
  # Guardar
  ggsave(file.path(path_salida, paste0("Map_SAE_", nombre_flota, ".png")), 
         final, width = 9, height = 12, dpi = 300, bg = "white")
  
  message("Mapa guardado para: ", nombre_flota)
}

# --- 5. EJECUCIÓN DEL BUCLE ---
flotas_aejecutar <- unique(map_total$Fleet)
lapply(flotas_aejecutar, mapear_flota)